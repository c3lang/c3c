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

bool c_find_member_index_rec(Decl *parent_decl, Decl *member, int *out_indices, int *out_depth, int max_depth)
{
	if (!parent_decl || !member || !decl_has_members(parent_decl) || !parent_decl->strukt.members)
	{
		return false;
	}

	Decl *unwrapped_member = c_decl_unwrap(member);

	FOREACH_IDX(i1, Decl *, m, parent_decl->strukt.members)
	{
		if (c_decl_unwrap(m) == unwrapped_member)
		{
			if (*out_depth < max_depth)
			{
				out_indices[(*out_depth)++] = (int)i1;
				return true;
			}
		}
	}

	FOREACH_IDX(i2, Decl *, m, parent_decl->strukt.members)
	{
		if (m->name && member->name && strcmp(m->name, member->name) == 0)
		{
			if (*out_depth < max_depth)
			{
				out_indices[(*out_depth)++] = (int)i2;
				return true;
			}
		}
	}

	FOREACH_IDX(i3, Decl *, m, parent_decl->strukt.members)
	{
		bool is_sub = (m->decl_kind == DECL_STRUCT || m->decl_kind == DECL_UNION || m->decl_kind == DECL_BITSTRUCT || m->is_substruct || !m->name);
		if (is_sub)
		{
			Decl *sub_decl = (m->decl_kind == DECL_STRUCT || m->decl_kind == DECL_UNION || m->decl_kind == DECL_BITSTRUCT)
			                     ? m
			                     : (m->type ? type_flatten(m->type)->decl : NULL);
			if (!sub_decl) continue;
			int saved_depth = *out_depth;
			if (*out_depth < max_depth)
			{
				out_indices[(*out_depth)++] = (int)i3;
				if (c_find_member_index_rec(sub_decl, member, out_indices, out_depth, max_depth))
				{
					return true;
				}
				*out_depth = saved_depth;
			}
		}
	}
	return false;
}

bool c_is_bitstruct_array(Type *type, Decl **out_decl)
{
	Type *flat = c_flatten_deref(type);
	if (!flat || flat->type_kind != TYPE_BITSTRUCT || !flat->decl)
	{
		return false;
	}
	Decl *d = flat->decl;
	if (out_decl)
	{
		*out_decl = d;
	}
	if (d->strukt.container_type)
	{
		Type *cflat = type_flatten(d->strukt.container_type->type);
		if (cflat && cflat->type_kind == TYPE_ARRAY)
		{
			return true;
		}
	}
	return false;
}

bool c_is_bitstruct_big_endian(Decl *d)
{
	if (!d)
	{
		return false;
	}
	if (d->strukt.big_endian)
	{
		return true;
	}
	if (d->strukt.little_endian)
	{
		return false;
	}
	return compiler.platform.big_endian;
}

bool c_is_bitstruct_requires_byteswap(Decl *d)
{
	if (!d || d->decl_kind != DECL_BITSTRUCT)
	{
		return false;
	}
	bool big_endian = compiler.platform.big_endian;
	if (d->strukt.big_endian)
	{
		return !big_endian;
	}
	if (d->strukt.little_endian)
	{
		return big_endian;
	}
	return false;
}

static int c_eval_bit_const_expr(Expr *e)
{
	while (e && (e->expr_kind == EXPR_RVALUE || e->expr_kind == EXPR_RECAST || e->expr_kind == EXPR_EXT_TRUNC))
	{
		e = e->inner_expr;
	}
	if (!e || e->expr_kind != EXPR_CONST)
	{
		return 0;
	}
	switch (e->const_expr.const_kind)
	{
		case CONST_INTEGER:
			return (int)e->const_expr.ixx.i.low;
		case CONST_ENUM:
			return (int)e->const_expr.enum_val->enum_constant.inner_ordinal;
		case CONST_BOOL:
			return e->const_expr.b ? 1 : 0;
		default:
			return 0;
	}
}

void c_get_bitstruct_member_bits(Decl *m, int *out_start_bit, int *out_end_bit)
{
	if (!m || m->decl_kind != DECL_VAR)
	{
		*out_start_bit = 0;
		*out_end_bit   = 0;
		return;
	}
	if (!m->var.bit_is_expr)
	{
		*out_start_bit = m->var.start_bit;
		*out_end_bit   = m->var.end_bit;
		return;
	}

	int s = c_eval_bit_const_expr(m->var.start);
	int e = m->var.end ? c_eval_bit_const_expr(m->var.end) : s;

	if (s < 0)
	{
		s = 0;
	}
	if (e < s)
	{
		e = s;
	}
	*out_start_bit = s;
	*out_end_bit   = e;
}

// A C expression for the low `bits` bits set, in the width the field is computed in (uint64_t up to 64 bits).
static const char *c_bit_mask_str(int bits)
{
	if (bits > 64) return str_printf("((((__c3_uint128)1) << %d) - 1)", bits);
	if (bits == 64) return "0xFFFFFFFFFFFFFFFFULL";
	return str_printf("0x%" PRIx64 "ULL", ((uint64_t)1 << bits) - 1);
}

void c_emit_bitstruct_accessors_to_file(FILE *f, Decl *decl)
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
	bool reverse        = c_is_bitstruct_big_endian(decl);
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
		int start_bit = 0, end_bit = 0;
		c_get_bitstruct_member_bits(m, &start_bit, &end_bit);
		int bit_size = end_bit - start_bit + 1;
		if (bit_size <= 0)
		{
			bit_size = 1;
		}
		bool is_signed         = type_is_signed(m_type);
		const char *shift_type = (bit_size > 64) ? "__c3_uint128" : "uint64_t";

		fprintf(f, "static inline %s %s_get_%s(const void *p) {\n", ret_tname, bs_name, m_name);
		if (is_array)
		{
			fputs("\tconst uint8_t *b = (const uint8_t *)p;\n", f);
			char expr_buf[2048];
			expr_buf[0]    = '\0';
			int term_count = 0;
			for (int bits_done = 0; bits_done < bit_size; )
			{
				CBitfieldStep s = c_bitfield_step(start_bit, bit_size, bits_done, reverse);
				char term[128];
				if (s.bit_in_byte == 0 && s.step_bits == 8)
				{
					if (s.shift > 0)
					{
						snprintf(term, sizeof(term), "((%s)b[%d] << %d)", shift_type, s.byte_idx, s.shift);
					}
					else
					{
						snprintf(term, sizeof(term), "(%s)b[%d]", shift_type, s.byte_idx);
					}
				}
				else
				{
					if (s.bit_in_byte > 0 && s.shift > 0)
					{
						snprintf(term, sizeof(term), "(((%s)(b[%d] >> %d & 0x%X) << %d))", shift_type, s.byte_idx, s.bit_in_byte, s.step_mask, s.shift);
					}
					else if (s.bit_in_byte > 0)
					{
						snprintf(term, sizeof(term), "((%s)(b[%d] >> %d & 0x%X))", shift_type, s.byte_idx, s.bit_in_byte, s.step_mask);
					}
					else if (s.shift > 0)
					{
						snprintf(term, sizeof(term), "(((%s)(b[%d] & 0x%X) << %d))", shift_type, s.byte_idx, s.step_mask, s.shift);
					}
					else
					{
						snprintf(term, sizeof(term), "((%s)(b[%d] & 0x%X))", shift_type, s.byte_idx, s.step_mask);
					}
				}
				if (term_count > 0)
				{
					strncat(expr_buf, " | ", sizeof(expr_buf) - strlen(expr_buf) - 1);
				}
				strncat(expr_buf, term, sizeof(expr_buf) - strlen(expr_buf) - 1);
				term_count++;
				bits_done += s.step_bits;
			}
			if (term_count == 0)
			{
				strcpy(expr_buf, "0");
			}
			if (is_signed && bit_size < 128)
			{
				fprintf(f, "\t%s res = %s;\n", shift_type, expr_buf);
				fprintf(f, "\tif (res & ((%s)1 << %d)) res |= ~(((%s)1 << %d) - 1);\n", shift_type, bit_size - 1, shift_type, bit_size);
				fprintf(f, "\treturn (%s)res;\n", ret_tname);
			}
			else
			{
				fprintf(f, "\treturn (%s)(%s);\n", ret_tname, expr_buf);
			}
		}
		else if (type_size(container_type) > 8)
		{
			fputs("\t__c3_uint128 v = *(const __c3_uint128 *)p;\n", f);
			if (c_is_bitstruct_requires_byteswap(decl))
			{
				fputs("\tv = (((__c3_uint128)__builtin_bswap64((uint64_t)v)) << 64) | ((__c3_uint128)__builtin_bswap64((uint64_t)(v >> 64)));\n", f);
			}
			if (bit_size >= 128)
			{
				fprintf(f, "\treturn (%s)v;\n", ret_tname);
			}
			else
			{
				fprintf(f, "\t%s res = (%s)((v >> %d) & %s);\n", shift_type, shift_type, start_bit, c_bit_mask_str(bit_size));
				if (is_signed)
				{
					fprintf(f, "\tif (res & ((%s)1 << %d)) res |= ~(%s)%s;\n", shift_type, bit_size - 1, shift_type, c_bit_mask_str(bit_size));
				}
				fprintf(f, "\treturn (%s)res;\n", ret_tname);
			}
		}
		else
		{
			const char *c_tname = c_type_name(NULL, container_type);
			uint64_t mask       = (bit_size >= 64) ? ~0ULL : (((uint64_t)1 << bit_size) - 1);
			fprintf(f, "\tuint64_t v = (uint64_t)*(const %s *)p;\n", c_tname);
			if (c_is_bitstruct_requires_byteswap(decl))
			{
				fprintf(f, "\tif (sizeof(%s) == 2) v = __builtin_bswap16((uint16_t)v);\n", c_tname);
				fprintf(f, "\telse if (sizeof(%s) == 4) v = __builtin_bswap32((uint32_t)v);\n", c_tname);
				fprintf(f, "\telse if (sizeof(%s) == 8) v = __builtin_bswap64((uint64_t)v);\n", c_tname);
			}
			if (start_bit == 0)
			{
				fprintf(f, "\tuint64_t res = v & 0x%" PRIx64 "ULL;\n", mask);
			}
			else
			{
				fprintf(f, "\tuint64_t res = (v >> %d) & 0x%" PRIx64 "ULL;\n", start_bit, mask);
			}
			if (is_signed && bit_size < 64)
			{
				fprintf(f, "\tif (res & (1ULL << %d)) res |= ~0x%" PRIx64 "ULL;\n", bit_size - 1, mask);
			}
			fprintf(f, "\treturn (%s)res;\n", ret_tname);
		}
		fputs("}\n", f);

		fprintf(f, "static inline void %s_set_%s(void *p, %s val) {\n", bs_name, m_name, ret_tname);
		if (is_array)
		{
			fputs("\tuint8_t *b = (uint8_t *)p;\n", f);
			for (int bits_done = 0; bits_done < bit_size; )
			{
				CBitfieldStep s = c_bitfield_step(start_bit, bit_size, bits_done, reverse);
				if (s.bit_in_byte == 0 && s.step_bits == 8)
				{
					if (s.shift > 0)
					{
						fprintf(f, "\tb[%d] = (uint8_t)((%s)val >> %d);\n", s.byte_idx, shift_type, s.shift);
					}
					else
					{
						fprintf(f, "\tb[%d] = (uint8_t)val;\n", s.byte_idx);
					}
				}
				else
				{
					if (s.shift > 0)
					{
						fprintf(f, "\tb[%d] = (b[%d] & ~0x%02X) | (((uint8_t)((%s)val >> %d) & 0x%02X) << %d);\n",
						        s.byte_idx, s.byte_idx, s.byte_mask, shift_type, s.shift, s.step_mask, s.bit_in_byte);
					}
					else
					{
						fprintf(f, "\tb[%d] = (b[%d] & ~0x%02X) | (((uint8_t)val & 0x%02X) << %d);\n",
						        s.byte_idx, s.byte_idx, s.byte_mask, s.step_mask, s.bit_in_byte);
					}
				}
				bits_done += s.step_bits;
			}
		}
		else if (type_size(container_type) > 8)
		{
			fputs("\t__c3_uint128 *v = (__c3_uint128 *)p;\n", f);
			if (c_is_bitstruct_requires_byteswap(decl))
			{
				fputs("\t__c3_uint128 _cur = (((__c3_uint128)__builtin_bswap64((uint64_t)*v)) << 64) | ((__c3_uint128)__builtin_bswap64((uint64_t)(*v >> 64)));\n", f);
				if (bit_size >= 128)
				{
					fprintf(f, "\t_cur = (__c3_uint128)val;\n");
				}
				else
				{
					fprintf(f, "\t__c3_uint128 mask = ((__c3_uint128)%s) << %d;\n", c_bit_mask_str(bit_size), start_bit);
					fprintf(f, "\t_cur = (_cur & ~mask) | ((((__c3_uint128)val) & %s) << %d);\n", c_bit_mask_str(bit_size), start_bit);
				}
				fputs("\t*v = (((__c3_uint128)__builtin_bswap64((uint64_t)_cur)) << 64) | ((__c3_uint128)__builtin_bswap64((uint64_t)(_cur >> 64)));\n", f);
			}
			else
			{
				if (bit_size >= 128)
				{
					fprintf(f, "\t*v = (__c3_uint128)val;\n");
				}
				else
				{
					fprintf(f, "\t__c3_uint128 mask = ((__c3_uint128)%s) << %d;\n", c_bit_mask_str(bit_size), start_bit);
					fprintf(f, "\t*v = (*v & ~mask) | ((((__c3_uint128)val) & %s) << %d);\n", c_bit_mask_str(bit_size), start_bit);
				}
			}
		}
		else
		{
			const char *c_tname   = c_type_name(NULL, container_type);
			uint64_t mask         = (bit_size >= 64) ? ~0ULL : (((uint64_t)1 << bit_size) - 1);
			uint64_t shifted_mask = (bit_size >= 64) ? ~0ULL : (mask << start_bit);
			fprintf(f, "\t%s *v = (%s *)p;\n", c_tname, c_tname);
			if (c_is_bitstruct_requires_byteswap(decl))
			{
				fprintf(f, "\tuint64_t _cur = (uint64_t)*v;\n");
				fprintf(f, "\tif (sizeof(%s) == 2) _cur = __builtin_bswap16((uint16_t)_cur);\n", c_tname);
				fprintf(f, "\telse if (sizeof(%s) == 4) _cur = __builtin_bswap32((uint32_t)_cur);\n", c_tname);
				fprintf(f, "\telse if (sizeof(%s) == 8) _cur = __builtin_bswap64((uint64_t)_cur);\n", c_tname);
				if (start_bit == 0)
				{
					fprintf(f, "\t_cur = (_cur & ~0x%" PRIx64 "ULL) | ((uint64_t)(%s)val & 0x%" PRIx64 "ULL);\n", shifted_mask, c_tname, mask);
				}
				else
				{
					fprintf(f, "\t_cur = (_cur & ~0x%" PRIx64 "ULL) | (((uint64_t)(%s)val & 0x%" PRIx64 "ULL) << %d);\n", shifted_mask, c_tname, mask, start_bit);
				}
				fprintf(f, "\tif (sizeof(%s) == 2) _cur = __builtin_bswap16((uint16_t)_cur);\n", c_tname);
				fprintf(f, "\telse if (sizeof(%s) == 4) _cur = __builtin_bswap32((uint32_t)_cur);\n", c_tname);
				fprintf(f, "\telse if (sizeof(%s) == 8) _cur = __builtin_bswap64((uint64_t)_cur);\n", c_tname);
				fprintf(f, "\t*v = (%s)_cur;\n", c_tname);
			}
			else
			{
				if (start_bit == 0)
				{
					fprintf(f, "\t*v = (%s)((*v & ~0x%" PRIx64 "ULL) | ((uint64_t)(%s)val & 0x%" PRIx64 "ULL));\n", c_tname, shifted_mask, c_tname, mask);
				}
				else
				{
					fprintf(f, "\t*v = (%s)((*v & ~0x%" PRIx64 "ULL) | (((uint64_t)(%s)val & 0x%" PRIx64 "ULL) << %d));\n", c_tname, shifted_mask, c_tname, mask, start_bit);
				}
			}
		}
		fputs("}\n\n", f);
	}
}

const char *c_emit_bitstruct_container(GenContext *c, Expr *parent_expr, Decl *member, int *out_container_var, Decl **out_bitstruct_decl)
{
	Type *parent_type  = c_expr_type(parent_expr);
	CValue parent_addr = {0};
	c_emit_lvalue_addr(c, parent_expr, &parent_addr, type_get_ptr(parent_type));

	Decl *parent_decl = get_struct_decl_from_type(parent_expr ? parent_expr->type : parent_type);
	int indices[16];
	int depth = 0;
	if (parent_decl)
	{
		c_find_member_index_rec(parent_decl, member, indices, &depth, 16);
	}

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
		for (int i = 0; i < depth - 1; i++)
		{
			PRINTF(".m%d", indices[i]);
		}
		PRINT(");\n");
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

static void c_emit_struct_member_addr(GenContext *c, const char *tname, int temp, Type *struct_type, int ptr_var, Decl *member)
{
	Decl *parent_decl = get_struct_decl_from_type(struct_type);
	int indices[16];
	int depth = 0;
	if (parent_decl)
	{
		c_find_member_index_rec(parent_decl, member, indices, &depth, 16);
	}
	if (depth == 0 || !parent_decl)
	{
		if (member && member->decl_kind == DECL_FUNC)
		{
			PRINTF("%s ___var_%d = (%s)&%s;\n", tname, temp, tname, c_decl_ref(c, member));
			return;
		}
		PRINTF("%s ___var_%d = (%s)___var_%d;\n", tname, temp, tname, ptr_var);
		return;
	}
	PRINTF("%s ___var_%d = (%s)&((*((%s*)___var_%d))",
	       tname, temp, tname, c_type_name(c, struct_type), ptr_var);
	for (int i = 0; i < depth; i++)
	{
		PRINTF(".m%d", indices[i]);
	}
	PRINT(");\n");
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

void c_emit_lvalue_addr(GenContext *c, Expr *expr, CValue *out_val, Type *target_type)
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
	if (target_type->type_kind == TYPE_VOID)
	{
		target_type = type_voidptr;
	}
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
			c_emit_struct_member_addr(c, tname, temp, base_struct_type, parent_val.var, member);
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
		c_emit_struct_member_addr(c, tname, temp, parent_type, parent_addr.var, member);
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
		const char *len = NULL;
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
			base = str_printf("((%s*)___var_%d)->ptr", c_type_name(c, parent_type), parent_addr.var);
			// A flexible array member has no known length.
			if (parent_type->type_kind != TYPE_FLEXIBLE_ARRAY) len = str_printf("%lluULL", (unsigned long long)parent_type->array.len);
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
	if (target_type->type_kind == TYPE_VOID)
	{
		target_type = type_int;
	}

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
	if (c_type_is_aggregate(target_type))
	{
		if (addr_val.var != 0)
		{
			PRINTF("__c3_memcpy(&___var_%d, (void*)___var_%d, sizeof(%s));\n",
			       temp, addr_val.var, tname);
		}
		else
		{
			PRINTF("__c3_memset(&___var_%d, 0, sizeof(%s));\n", temp, tname);
		}
	}
	else
	{
		if (addr_val.var != 0)
		{
			if (type_size(target_type) == 16 && (target_type->type_kind == TYPE_I128 || target_type->type_kind == TYPE_U128))
			{
				PRINTF("___var_%d = 0;\n", temp);
				PRINTF("__c3_memcpy(&___var_%d, (void*)___var_%d, sizeof(%s));\n", temp, addr_val.var, tname);
			}
			else
			{
				PRINTF("___var_%d = *(%s*)___var_%d;\n", temp, tname, addr_val.var);
			}
		}
		else
		{
			PRINTF("___var_%d = 0;\n", temp);
		}
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
					PRINTF("((%s*)___var_%d)->ptr[%d] = ((%s*)___var_%d)->ptr[%d];\n", ptname, parent_addr.var, p_idx, c_type_name(c, right_val->type), right_val->var, i);
				}
				else
				{
					PRINTF("((%s*)___var_%d)->ptr[%d] = ___var_%d.ptr[%d];\n", ptname, parent_addr.var, p_idx, right_val->var, i);
				}
			}
			else if (right_val && right_val->var != 0)
			{
				PRINTF("((%s*)___var_%d)->ptr[%d] = ___var_%d;\n", ptname, parent_addr.var, p_idx, right_val->var);
			}
			else
			{
				PRINTF("((%s*)___var_%d)->ptr[%d] = 0;\n", ptname, parent_addr.var, p_idx);
			}
		}
		if (out_val)
		{
			int res_temp = c_emit_temp_var(c, out_val, target_type);
			PRINTF("%s ___var_%d = {0};\n", c_type_name(c, target_type), res_temp);
			for (int i = 0; i < sw_len; i++)
			{
				int p_idx = SWIZZLE_INDEX(sw[i]);
				PRINTF("___var_%d.ptr[%d] = ((%s*)___var_%d)->ptr[%d];\n", res_temp, i, ptname, parent_addr.var, p_idx);
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
	if (right_val && right_val->var && (target_type->type_kind == TYPE_I128 || target_type->type_kind == TYPE_U128))
	{
		// The address may not be 16 byte aligned.
		ASSERT(strcmp(tname, c_type_name(c, c_safe_type_lower(right_val->type))) == 0);
		c_value_rvalue(c, right_val);
		PRINTF("__c3_memcpy((void*)___var_%d, &___var_%d, sizeof(%s));\n", addr_val.var, right_val->var, tname);
	}
	else
	{
		c_emit_store(c, str_printf("(*(%s*)___var_%d)", tname, addr_val.var), target_type, right_val);
	}
	c_assign_result(c, target_type, right_val, out_val);
}