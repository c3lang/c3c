#include "c_codegen_internal.h"

Decl *get_struct_decl_from_type(Type *type)
{
	if (!type || !is_valid_type_ptr(type))
	{
		return NULL;
	}
	Type *flat = type_flatten(type);
	if (flat && flat->type_kind == TYPE_POINTER && flat->pointer && is_valid_type_ptr(flat->pointer))
	{
		flat = type_flatten(flat->pointer);
	}
	if (!flat)
	{
		return NULL;
	}
	if (flat->type_kind == TYPE_STRUCT || flat->type_kind == TYPE_UNION || flat->type_kind == TYPE_BITSTRUCT)
	{
		Decl *d = flat->decl;
		if (d && (d->decl_kind == DECL_STRUCT || d->decl_kind == DECL_UNION || d->decl_kind == DECL_BITSTRUCT))
		{
			return d;
		}
	}
	return NULL;
}

bool c_find_member_index_rec(Decl *parent_decl, Decl *member, int *out_indices, int *out_depth, int max_depth)
{
	if (!parent_decl || !member || !decl_has_members(parent_decl) || !parent_decl->strukt.members)
	{
		return false;
	}

	// 1. Check direct members first by pointer equality
	{
		FOREACH_IDX(i1, Decl *, m, parent_decl->strukt.members)
		{
			if (m == member)
			{
				if (*out_depth < max_depth)
				{
					out_indices[(*out_depth)++] = (int)i1;
					return true;
				}
			}
		}
	}

	// 2. Check direct members by name
	{
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
	}

	// 3. Recurse into substructs if not found in the current struct
	{
		FOREACH_IDX(i3, Decl *, m, parent_decl->strukt.members)
		{
			if (m->decl_kind == DECL_STRUCT || m->decl_kind == DECL_UNION || m->decl_kind == DECL_BITSTRUCT ||
			    (m->decl_kind == DECL_VAR && is_valid_type_ptr(m->type) &&
			     (type_flatten(m->type)->type_kind == TYPE_STRUCT || type_flatten(m->type)->type_kind == TYPE_UNION || type_flatten(m->type)->type_kind == TYPE_BITSTRUCT)))
			{
				Decl *sub_decl  = (m->decl_kind == DECL_STRUCT || m->decl_kind == DECL_UNION || m->decl_kind == DECL_BITSTRUCT)
				                      ? m
				                      : type_flatten(m->type)->decl;
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
	}
	return false;
}

bool c_is_bitstruct_array(Type *type, Decl **out_decl)
{
	if (!type || !is_valid_type_ptr(type))
	{
		return false;
	}
	Type *flat = type_flatten(type);
	if (flat && flat->type_kind == TYPE_POINTER && flat->pointer && is_valid_type_ptr(flat->pointer))
	{
		flat = type_flatten(flat->pointer);
	}
	if (!flat || flat->type_kind != TYPE_BITSTRUCT || !flat->decl)
	{
		return false;
	}
	Decl *d = flat->decl;
	if (out_decl)
	{
		*out_decl = d;
	}
	if (d->strukt.container_type && is_valid_type_ptr(d->strukt.container_type->type))
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

	Expr *start = m->var.start;
	while (start && (start->expr_kind == EXPR_RVALUE || start->expr_kind == EXPR_RECAST || start->expr_kind == EXPR_EXT_TRUNC))
	{
		start = start->inner_expr;
	}

	int s = 0;
	if (start && start->expr_kind == EXPR_CONST)
	{
		if (start->const_expr.const_kind == CONST_INTEGER)
		{
			s = (int)start->const_expr.ixx.i.low;
		}
		else if (start->const_expr.const_kind == CONST_ENUM)
		{
			s = (int)start->const_expr.enum_val->enum_constant.inner_ordinal;
		}
		else if (start->const_expr.const_kind == CONST_BOOL)
		{
			s = start->const_expr.b ? 1 : 0;
		}
	}

	int e     = s;
	Expr *end = m->var.end;
	while (end && (end->expr_kind == EXPR_RVALUE || end->expr_kind == EXPR_RECAST || end->expr_kind == EXPR_EXT_TRUNC))
	{
		end = end->inner_expr;
	}

	if (end && end->expr_kind == EXPR_CONST)
	{
		if (end->const_expr.const_kind == CONST_INTEGER)
		{
			e = (int)end->const_expr.ixx.i.low;
		}
		else if (end->const_expr.const_kind == CONST_ENUM)
		{
			e = (int)end->const_expr.enum_val->enum_constant.inner_ordinal;
		}
		else if (end->const_expr.const_kind == CONST_BOOL)
		{
			e = end->const_expr.b ? 1 : 0;
		}
	}

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

void c_emit_bitstruct_accessors_to_file(FILE *f, Decl *decl)
{
	if (!decl || decl->decl_kind != DECL_BITSTRUCT || !decl->strukt.members)
	{
		return;
	}
	if (!decl->strukt.container_type || !is_valid_type_ptr(decl->strukt.container_type->type))
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

		// Getter
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
			if (is_signed && bit_size < 64)
			{
				fprintf(f, "\tuint64_t res = %s;\n", expr_buf);
				fprintf(f, "\tif (res & (1ULL << %d)) res |= ~((1ULL << %d) - 1);\n", bit_size - 1, bit_size);
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
				uint64_t mask = (bit_size >= 64) ? ~0ULL : (((uint64_t)1 << bit_size) - 1);
				fprintf(f, "\tuint64_t res = (uint64_t)((v >> %d) & 0x%" PRIx64 "ULL);\n", start_bit, mask);
				if (is_signed && bit_size < 64)
				{
					fprintf(f, "\tif (res & (1ULL << %d)) res |= ~0x%" PRIx64 "ULL;\n", bit_size - 1, mask);
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

		// Setter
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
					uint64_t mask = (bit_size >= 64) ? ~0ULL : (((uint64_t)1 << bit_size) - 1);
					fprintf(f, "\t__c3_uint128 mask = ((__c3_uint128)0x%" PRIx64 "ULL) << %d;\n", mask, start_bit);
					fprintf(f, "\t_cur = (_cur & ~mask) | ((((__c3_uint128)val) & 0x%" PRIx64 "ULL) << %d);\n", mask, start_bit);
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
					uint64_t mask = (bit_size >= 64) ? ~0ULL : (((uint64_t)1 << bit_size) - 1);
					fprintf(f, "\t__c3_uint128 mask = ((__c3_uint128)0x%" PRIx64 "ULL) << %d;\n", mask, start_bit);
					fprintf(f, "\t*v = (*v & ~mask) | ((((__c3_uint128)val) & 0x%" PRIx64 "ULL) << %d);\n", mask, start_bit);
				}
			}
		}
		else
		{
			const char *c_tname   = c_type_name(NULL, container_type);
			uint64_t mask         = (bit_size >= 64) ? ~0ULL : (((uint64_t)1 << bit_size) - 1);
			uint64_t shifted_mask = (bit_size + start_bit >= 64) ? ~0ULL : (mask << start_bit);
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
			if (container_decl->decl_kind == DECL_BITSTRUCT && container_decl->strukt.container_type && is_valid_type_ptr(container_decl->strukt.container_type->type))
			{
				c_type = c_safe_type_lower(container_decl->strukt.container_type->type);
			}
			else if (container_decl->type && is_valid_type_ptr(container_decl->type))
			{
				Type *ft = type_flatten(container_decl->type);
				if (ft->type_kind == TYPE_BITSTRUCT && ft->decl && ft->decl->strukt.container_type && is_valid_type_ptr(ft->decl->strukt.container_type->type))
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

	Type *flat = type_flatten(parent_type);
	if (flat && flat->type_kind == TYPE_POINTER && flat->pointer)
	{
		flat = type_flatten(flat->pointer);
	}
	container_decl = (flat && flat->type_kind == TYPE_BITSTRUCT) ? flat->decl : parent_decl;

	Type *c_type = type_uint;
	if (container_decl)
	{
		if (container_decl->decl_kind == DECL_BITSTRUCT && container_decl->strukt.container_type && is_valid_type_ptr(container_decl->strukt.container_type->type))
		{
			c_type = c_safe_type_lower(container_decl->strukt.container_type->type);
		}
		else if (container_decl->type && is_valid_type_ptr(container_decl->type))
		{
			Type *ft = type_flatten(container_decl->type);
			if (ft->type_kind == TYPE_BITSTRUCT && ft->decl && ft->decl->strukt.container_type && is_valid_type_ptr(ft->decl->strukt.container_type->type))
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
			PRINTF("%s ___var_%d = (%s)&%s;\n", tname, temp, tname, c_get_decl_name(member));
			return;
		}
		PRINTF("%s ___var_%d = (%s)___var_%d;\n", tname, temp, tname, ptr_var);
		return;
	}
	PRINTF("%s ___var_%d = (___var_%d != NULL) ? ((%s)&((*((%s*)___var_%d))",
	       tname, temp, ptr_var, tname, c_type_name(c, struct_type), ptr_var);
	for (int i = 0; i < depth; i++)
	{
		PRINTF(".m%d", indices[i]);
	}
	PRINT(")) : NULL;\n");
}

void c_emit_lvalue_addr(GenContext *c, Expr *expr, CValue *out_val, Type *target_type)
{
	target_type = (target_type && is_valid_type_ptr(target_type)) ? c_safe_type_lower(target_type) : type_void;
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
				PRINTF("%s ___var_%d = (%s)&%s;\n", tname, temp, tname, c_get_decl_name(d));
				return;
			}
			if (c_is_file_global(d))
			{
				c_emit_global_decl(c, d);
				PRINTF("%s ___var_%d = (%s)&%s;\n", tname, temp, tname, c_get_decl_name(d));
			}
			else
			{
				VariableId vid = c_get_or_create_decl_var(c, d);
				PRINTF("%s ___var_%d = (%s)&___var_%d;\n", tname, temp, tname, vid);
			}
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
				const char *arr_name = c_get_enum_assoc_name(enum_decl, member);
				CValue parent_val    = {0};
				c_emit_expr(c, &parent_val, expr->access_resolved_expr.parent);
				c_value_rvalue(c, &parent_val);
				c_ensure_cvalue_var(c, &parent_val, type_sz);
				const char *idx_str = parent_is_ptr
				                          ? str_printf("(size_t)(*___var_%d)", parent_val.var)
				                          : str_printf("(size_t)___var_%d", parent_val.var);
				PRINTF("%s ___var_%d = (%s)&%s[%s];\n", tname, temp, tname, arr_name, idx_str);
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

			Type *base_struct_type = (parent_type->pointer && is_valid_type_ptr(parent_type->pointer)) ? c_safe_type_lower(parent_type->pointer) : type_void;
			if (base_struct_type->type_kind == TYPE_VOID)
			{
				base_struct_type = parent_type;
			}
			if (base_struct_type->type_kind == TYPE_INTERFACE || base_struct_type->type_kind == TYPE_ANY)
			{
				if (member && member->decl_kind == DECL_FUNC)
				{
					const char *disp_resolver = c_intern(str_printf("__c3_dyn_resolve_%s", c_get_decl_name(member)));
					PRINTF("%s ___var_%d = (%s)%s((c3typeid_t)(((__c3_any__*)___var_%d)->typeid));\n", tname, temp, tname, disp_resolver, parent_val.var);
					return;
				}
				const char *fname = (member && member->name && (strcmp(member->name, "typeid") == 0 || strcmp(member->name, "type") == 0)) ? "typeid" : "ptr";
				PRINTF("%s ___var_%d = (___var_%d != NULL) ? ((%s)&(((__c3_any__*)___var_%d)->%s)) : NULL;\n", tname, temp, parent_val.var, tname, parent_val.var, fname);
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
				PRINTF("%s ___var_%d = (%s)&%s;\n", tname, temp, tname, c_get_decl_name(member));
				return;
			}
			const char *fname = (member && member->name && strcmp(member->name, "len") == 0) ? "len" : "ptr";
			PRINTF("%s ___var_%d = (%s)&(___var_%d.%s);\n", tname, temp, tname, parent_val.var, fname);
			return;
		}
		if (parent_type->type_kind == TYPE_ANY || parent_type->type_kind == TYPE_INTERFACE)
		{
			CValue parent_val = {0};
			c_emit_expr(c, &parent_val, expr->access_resolved_expr.parent);
			c_value_rvalue(c, &parent_val);
			c_ensure_cvalue_var(c, &parent_val, parent_type);

			if (member && member->decl_kind == DECL_FUNC)
			{
				const char *disp_resolver = c_intern(str_printf("__c3_dyn_resolve_%s", c_get_decl_name(member)));
				PRINTF("%s ___var_%d = (%s)%s((c3typeid_t)___var_%d.typeid);\n", tname, temp, tname, disp_resolver, parent_val.var);
				return;
			}
			const char *fname = (member && member->name && (strcmp(member->name, "typeid") == 0 || strcmp(member->name, "type") == 0)) ? "typeid" : "ptr";
			PRINTF("%s ___var_%d = (%s)&(___var_%d.%s);\n", tname, temp, tname, parent_val.var, fname);
			return;
		}
		CValue parent_addr = {0};
		c_emit_lvalue_addr(c, expr->access_resolved_expr.parent, &parent_addr, type_get_ptr(parent_type));
		c_ensure_cvalue_var(c, &parent_addr, type_get_ptr(parent_type));
		c_emit_struct_member_addr(c, tname, temp, parent_type, parent_addr.var, member);
		return;
	}
	if (expr->expr_kind == EXPR_SUBSCRIPT || expr->expr_kind == EXPR_SUBSCRIPT_ADDR)
	{
		Expr *parent_expr = exprptr(expr->subscript_expr.expr);
		Type *parent_type = c_expr_type(parent_expr);
		CValue index_val  = {0};
		c_emit_expr(c, &index_val, exprptr(expr->subscript_expr.index.expr));
		c_value_rvalue(c, &index_val);
		c_ensure_cvalue_var(c, &index_val, type_sz);

		if (parent_type->type_kind == TYPE_POINTER)
		{
			CValue parent_val = {0};
			c_emit_expr(c, &parent_val, parent_expr);
			c_value_rvalue(c, &parent_val);
			Type *pt = (parent_type->pointer && is_valid_type_ptr(parent_type->pointer)) ? c_safe_type_lower(parent_type->pointer) : NULL;
			if (pt && pt->type_kind != TYPE_VOID)
			{
				PRINTF("%s ___var_%d = (%s)(((%s*)___var_%d) + ___var_%d);\n", tname, temp, tname, c_type_name(c, pt), parent_val.var, index_val.var);
			}
			else
			{
				PRINTF("%s ___var_%d = (%s)(((char*)___var_%d) + (___var_%d * sizeof(*(%s)0)));\n", tname, temp, tname, parent_val.var, index_val.var, tname);
			}
			return;
		}
		else if (parent_type->type_kind == TYPE_SLICE)
		{
			CValue parent_val = {0};
			c_emit_expr(c, &parent_val, parent_expr);
			c_value_rvalue(c, &parent_val);
			if (expr->subscript_expr.index.start_from_end)
			{
				PRINTF("%s ___var_%d = (%s)&(___var_%d.ptr[___var_%d.len - ___var_%d]);\n", tname, temp, tname, parent_val.var, parent_val.var, index_val.var);
			}
			else
			{
				PRINTF("%s ___var_%d = (%s)&(___var_%d.ptr[___var_%d]);\n", tname, temp, tname, parent_val.var, index_val.var);
			}
			return;
		}
		else
		{
			CValue parent_addr = {0};
			c_emit_lvalue_addr(c, parent_expr, &parent_addr, type_get_ptr(parent_type));
			if (expr->subscript_expr.index.start_from_end)
			{
				PRINTF("%s ___var_%d = (%s)&(((%s*)___var_%d)->ptr[%llu - ___var_%d]);\n",
				       tname, temp, tname, c_type_name(c, parent_type), parent_addr.var, (unsigned long long)parent_type->array.len, index_val.var);
			}
			else
			{
				PRINTF("%s ___var_%d = (%s)&(((%s*)___var_%d)->ptr[___var_%d]);\n",
				       tname, temp, tname, c_type_name(c, parent_type), parent_addr.var, index_val.var);
			}
			return;
		}
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
			PRINTF("%s ___var_%d = (%s)&___var_%d;\n", tname, temp, tname, inner_val.var);
		}
	}
	else
	{
		PRINTF("%s ___var_%d = NULL;\n", tname, temp);
	}
}

void c_emit_lvalue_read(GenContext *c, Expr *expr, CValue *out_val, Type *target_type)
{
	target_type = (target_type && is_valid_type_ptr(target_type)) ? c_safe_type_lower(target_type) : type_void;
	if (target_type->type_kind == TYPE_VOID && expr)
	{
		target_type = c_expr_type(expr);
	}
	if (target_type->type_kind == TYPE_VOID)
	{
		target_type = type_int;
	}

	int temp          = c_emit_temp_var(c, out_val, target_type);
	const char *tname = c_type_name(c, target_type);

	if (!expr)
	{
		c_emit_var_zero_init(c, temp, target_type);
		return;
	}
	if (expr->expr_kind == EXPR_RECAST || expr->expr_kind == EXPR_ADDR_CONVERSION || expr->expr_kind == EXPR_RVALUE)
	{
		CValue inner_val = {0};
		c_emit_expr(c, &inner_val, expr->inner_expr);
		c_value_rvalue(c, &inner_val);
		c_emit_assign_var(c, temp, target_type, &inner_val);
		return;
	}
	if (expr->expr_kind == EXPR_IDENTIFIER || expr->expr_kind == EXPR_DECL)
	{
		Decl *decl = (expr->expr_kind == EXPR_DECL) ? expr->decl_expr : c_decl_unwrap(expr->ident_expr);
		if (decl)
		{
			if (decl->decl_kind == DECL_VAR && decl->var.kind == VARDECL_PARAM_EXPR && decl->var.init_expr)
			{
				c_emit_lvalue_read(c, decl->var.init_expr, out_val, target_type);
				return;
			}
			if (decl->decl_kind == DECL_FUNC)
			{
				PRINTF("%s ___var_%d = (void*)&%s;\n", tname, temp, c_get_decl_name(decl));
				return;
			}
			if (decl->decl_kind == DECL_ENUM_CONSTANT)
			{
				PRINTF("%s ___var_%d = %d;\n", tname, temp, decl->enum_constant.inner_ordinal);
				return;
			}
			if (decl->decl_kind == DECL_FAULT)
			{
				const char *fsym = c_fault_symbol_name(decl);
				PRINTF("%s ___var_%d = (c3fault_t)%s;\n", tname, temp, fsym);
				return;
			}
			Decl *raw_ident = (expr->expr_kind == EXPR_DECL) ? expr->decl_expr : (expr->ident_expr ? decl_raw(expr->ident_expr) : NULL);
			if (c_is_file_global(decl))
			{
				c_emit_global_decl(c, decl);
				const char *gname = c_get_decl_name(decl);
				if (IS_OPTIONAL(decl) && (!raw_ident || raw_ident->var.kind != VARDECL_UNWRAPPED))
				{
					int fault_temp = c_emit_temp_var(c, NULL, type_fault);
					PRINTF("c3fault_t ___var_%d = %s__f;\n", fault_temp, gname);
					PRINTF("__c3_current_fault = ___var_%d;\n", fault_temp);
					out_val->optional = fault_temp;
				}
				if (c_type_is_aggregate(target_type))
				{
					PRINTF("%s ___var_%d;\n", tname, temp);
					PRINTF("__c3_memcpy(&___var_%d, &%s, sizeof(%s));\n", temp, gname, gname);
				}
				else
				{
					PRINTF("%s ___var_%d = %s;\n", tname, temp, gname);
				}
			}
			else
			{
				VariableId vid  = c_get_or_create_decl_var(c, decl);
				VariableId fvid = 0;
				if (raw_ident && raw_ident->decl_kind == DECL_VAR && raw_ident->var.kind != VARDECL_UNWRAPPED)
				{
					fvid = c_get_decl_fault_var(c, decl);
				}
				if (fvid != 0)
				{
					out_val->optional = fvid;
					PRINTF("__c3_current_fault = ___var_%d;\n", fvid);
				}
				if (c_type_is_aggregate(target_type))
				{
					PRINTF("%s ___var_%d;\n", tname, temp);
					PRINTF("__c3_memcpy(&___var_%d, &___var_%d, sizeof(___var_%d));\n", temp, vid, vid);
				}
				else
				{
					PRINTF("%s ___var_%d = ___var_%d;\n", tname, temp, vid);
				}
			}
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
				const char *arr_name = c_get_enum_assoc_name(enum_decl, member);
				CValue parent_val    = {0};
				c_emit_expr(c, &parent_val, expr->access_resolved_expr.parent);
				c_value_rvalue(c, &parent_val);
				c_ensure_cvalue_var(c, &parent_val, type_sz);
				const char *idx_str = parent_is_ptr
				                          ? str_printf("(size_t)(*___var_%d)", parent_val.var)
				                          : str_printf("(size_t)___var_%d", parent_val.var);
				if (c_type_is_aggregate(target_type))
				{
					PRINTF("%s ___var_%d;\n", tname, temp);
					PRINTF("__c3_memcpy(&___var_%d, &%s[%s], sizeof(%s));\n", temp, arr_name, idx_str, tname);
				}
				else
				{
					PRINTF("%s ___var_%d = (%s)%s[%s];\n", tname, temp, tname, arr_name, idx_str);
				}
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
			if (base_type && (base_type->type_kind == TYPE_ANY || base_type->type_kind == TYPE_INTERFACE))
			{
				CValue parent_val = {0};
				c_emit_expr(c, &parent_val, expr->access_resolved_expr.parent);
				c_value_rvalue(c, &parent_val);
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
			PRINTF("%s ___var_%d = (%s)&%s;\n", tname, temp, tname, c_get_decl_name(member));
			return;
		}
	}

	CValue addr_val = {0};
	c_emit_lvalue_addr(c, expr, &addr_val, type_get_ptr(target_type));
	if (c_type_is_aggregate(target_type))
	{
		PRINTF("%s ___var_%d;\n", tname, temp);
		if (addr_val.var != 0)
		{
			PRINTF("if (___var_%d != NULL) { __c3_memcpy(&___var_%d, (void*)___var_%d, sizeof(%s)); } else { __c3_memset(&___var_%d, 0, sizeof(%s)); }\n",
			       addr_val.var, temp, addr_val.var, tname, temp, tname);
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
			PRINTF("%s ___var_%d = (___var_%d != NULL) ? *(%s*)___var_%d : 0;\n", tname, temp, addr_val.var, tname, addr_val.var);
		}
		else
		{
			PRINTF("%s ___var_%d = 0;\n", tname, temp);
		}
	}
}

void c_emit_lvalue_assign(GenContext *c, Expr *left, CValue *right_val, const char *assign_op, CValue *out_val)
{
	if (!left)
	{
		return;
	}
	if (left->expr_kind == EXPR_RECAST || left->expr_kind == EXPR_ADDR_CONVERSION || left->expr_kind == EXPR_RVALUE)
	{
		c_emit_lvalue_assign(c, left->inner_expr, right_val, assign_op, out_val);
		return;
	}
	if (left->expr_kind == EXPR_SWIZZLE)
	{
		Expr *parent       = exprptr(left->swizzle_expr.parent);
		Type *parent_type  = c_expr_type(parent);
		CValue parent_addr = {0};
		c_emit_lvalue_addr(c, parent, &parent_addr, type_get_ptr(parent_type));
		const char *ptname = c_type_name(c, parent_type);
		const char *sw     = left->swizzle_expr.swizzle;
		int sw_len         = (int)strlen(sw);
		Type *target_type  = c_expr_type(left);
		bool r_is_vec      = (right_val && right_val->type && (right_val->type->type_kind == TYPE_ARRAY || right_val->type->type_kind == TYPE_VECTOR || right_val->type->type_kind == TYPE_SIMD_VECTOR));
		for (int i = 0; i < sw_len; i++)
		{
			int p_idx = SWIZZLE_INDEX(sw[i]);
			if (r_is_vec)
			{
				if (right_val->kind == CV_ADDRESS)
				{
					PRINTF("((%s*)___var_%d)->ptr[%d] %s ((%s*)___var_%d)->ptr[%d];\n", ptname, parent_addr.var, p_idx, assign_op, c_type_name(c, right_val->type), right_val->var, i);
				}
				else
				{
					PRINTF("((%s*)___var_%d)->ptr[%d] %s ___var_%d.ptr[%d];\n", ptname, parent_addr.var, p_idx, assign_op, right_val->var, i);
				}
			}
			else if (right_val && right_val->var != 0)
			{
				PRINTF("((%s*)___var_%d)->ptr[%d] %s ___var_%d;\n", ptname, parent_addr.var, p_idx, assign_op, right_val->var);
			}
			else
			{
				PRINTF("((%s*)___var_%d)->ptr[%d] %s 0;\n", ptname, parent_addr.var, p_idx, assign_op);
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
			c_emit_lvalue_assign(c, left_decl->var.init_expr, right_val, assign_op, out_val);
			return;
		}
		if (c_is_file_global(left_decl))
		{
			c_emit_global_decl(c, left_decl);
			const char *gname = c_get_decl_name(left_decl);
			if (IS_OPTIONAL(left_decl) && strcmp(assign_op, "=") == 0)
			{
				if (right_val && right_val->optional != 0)
				{
					PRINTF("%s__f = (c3fault_t)(uintptr_t)___var_%d;\n", gname, right_val->optional);
				}
				else if (right_val && right_val->type && (right_val->type->type_kind == TYPE_ANYFAULT || right_val->type == type_fault))
				{
					PRINTF("%s__f = (c3fault_t)(uintptr_t)___var_%d;\n", gname, right_val->var);
				}
				else
				{
					PRINTF("%s__f = __c3_current_fault;\n", gname);
				}
				PRINTF("__c3_current_fault = %s__f;\n", gname);
			}
			Type *gt = c_decl_type(left_decl);
			if (!right_val || right_val->var == 0)
			{
				if (c_type_is_aggregate(gt))
				{
					PRINTF("__c3_memset(&%s, 0, sizeof(%s));\n", gname, gname);
				}
				else if (type_is_pointer(gt))
				{
					PRINTF("%s %s NULL;\n", gname, assign_op);
				}
				else
				{
					PRINTF("%s %s 0;\n", gname, assign_op);
				}
				if (out_val)
				{
					int temp = c_emit_temp_var(c, out_val, gt);
					c_emit_var_zero_init(c, temp, gt);
				}
				return;
			}
			if (strcmp(assign_op, "=") != 0 && c_type_is_aggregate(gt) && (gt->type_kind == TYPE_ARRAY || gt->type_kind == TYPE_VECTOR || gt->type_kind == TYPE_SIMD_VECTOR))
			{
				int len = (int)gt->array.len;
				if (len <= 0)
				{
					len = 1;
				}
				bool r_is_agg = (right_val->type && c_type_is_aggregate(right_val->type));
				for (int k = 0; k < len; k++)
				{
					if (r_is_agg)
					{
						PRINTF("%s.ptr[%d] %s ___var_%d.ptr[%d];\n", gname, k, assign_op, right_val->var, k);
					}
					else
					{
						PRINTF("%s.ptr[%d] %s ___var_%d;\n", gname, k, assign_op, right_val->var);
					}
				}
				if (out_val)
				{
					int temp = c_emit_temp_var(c, out_val, gt);
					PRINTF("%s ___var_%d;\n", c_type_name(c, gt), temp);
					PRINTF("__c3_memcpy(&___var_%d, &%s, sizeof(%s));\n", temp, gname, gname);
				}
				return;
			}
			if (gt->type_kind == TYPE_POINTER && (right_val->type && (right_val->type->type_kind == TYPE_ANY || right_val->type->type_kind == TYPE_INTERFACE)))
			{
				PRINTF("%s %s (void*)___var_%d.ptr;\n", gname, assign_op, right_val->var);
			}
			else if (gt->type_kind == TYPE_POINTER && (right_val->type && right_val->type->type_kind == TYPE_SLICE))
			{
				PRINTF("%s %s (%s)___var_%d.ptr;\n", gname, assign_op, c_type_name(c, gt), right_val->var);
			}
			else if (gt->type_kind == TYPE_POINTER && (right_val->type && (right_val->type->type_kind == TYPE_ARRAY || right_val->type->type_kind == TYPE_VECTOR || right_val->type->type_kind == TYPE_SIMD_VECTOR)))
			{
				PRINTF("%s %s (%s)(___var_%d%sptr);\n", gname, assign_op, c_type_name(c, gt), right_val->var, c_arrow(right_val));
			}
			else if (gt->type_kind == TYPE_SLICE && (right_val->type && (right_val->type->type_kind == TYPE_ARRAY || right_val->type->type_kind == TYPE_VECTOR || right_val->type->type_kind == TYPE_SIMD_VECTOR)))
			{
				PRINTF("%s %s (%s){ .ptr = (void*)(___var_%d%sptr), .len = %llu };\n",
				       gname, assign_op, c_type_name(c, gt), right_val->var, c_arrow(right_val), (unsigned long long)right_val->type->array.len);
			}
			else if (gt->type_kind == TYPE_ANY || gt->type_kind == TYPE_INTERFACE)
			{
				c_emit_assign_to_any(c, gname, right_val);
			}
			else if (c_type_is_aggregate(gt))
			{
				if (right_val->kind == CV_ADDRESS)
				{
					PRINTF("__c3_memcpy(&%s, (void*)___var_%d, sizeof(%s));\n", gname, right_val->var, gname);
				}
				else
				{
					PRINTF("__c3_memcpy(&%s, &___var_%d, sizeof(%s));\n", gname, right_val->var, gname);
				}
			}
			else if (type_is_integer(gt) && (type_is_pointer(right_val->type) || right_val->type->type_kind == TYPE_ANYFAULT || right_val->type->type_kind == TYPE_TYPEID))
			{
				PRINTF("%s %s (%s)(uintptr_t)___var_%d;\n", gname, assign_op, c_type_name(c, gt), right_val->var);
			}
			else if ((type_is_pointer(gt) || gt->type_kind == TYPE_ANYFAULT || gt->type_kind == TYPE_TYPEID) && type_is_integer(right_val->type))
			{
				PRINTF("%s %s (%s)(uintptr_t)___var_%d;\n", gname, assign_op, c_type_name(c, gt), right_val->var);
			}
			else if (type_is_float(gt) && (type_is_pointer(right_val->type) || right_val->type->type_kind == TYPE_ANYFAULT || right_val->type->type_kind == TYPE_TYPEID))
			{
				PRINTF("%s %s 0.0;\n", gname, assign_op);
			}
			else if ((type_is_pointer(gt) || gt->type_kind == TYPE_ANYFAULT || gt->type_kind == TYPE_TYPEID) && type_is_float(right_val->type))
			{
				PRINTF("%s %s NULL;\n", gname, assign_op);
			}
			else if (gt != right_val->type)
			{
				PRINTF("%s %s (%s)___var_%d;\n", gname, assign_op, c_type_name(c, gt), right_val->var);
			}
			else if (type_is_pointer(gt) && gt->pointer && gt->pointer->type_kind == TYPE_VOID && (strcmp(assign_op, "+=") == 0 || strcmp(assign_op, "-=") == 0))
			{
				const char *arith_op = (strcmp(assign_op, "+=") == 0) ? "+" : "-";
				PRINTF("%s = (void*)((char*)%s %s ___var_%d);\n", gname, gname, arith_op, right_val->var);
			}
			else
			{
				PRINTF("%s %s ___var_%d;\n", gname, assign_op, right_val->var);
			}
			if (out_val)
			{
				if (strcmp(assign_op, "=") == 0 && right_val)
				{
					*out_val = *right_val;
				}
				else
				{
					int temp = c_emit_temp_var(c, out_val, gt);
					if (c_type_is_aggregate(gt))
					{
						PRINTF("%s ___var_%d;\n", c_type_name(c, gt), temp);
						PRINTF("__c3_memcpy(&___var_%d, &%s, sizeof(%s));\n", temp, gname, gname);
					}
					else
					{
						PRINTF("%s ___var_%d = %s;\n", c_type_name(c, gt), temp, gname);
					}
				}
			}
			return;
		}
		else
		{
			VariableId vid = c_get_or_create_decl_var(c, left_decl);
			Type *lt       = c_decl_type(left_decl);
			if (strcmp(assign_op, "=") == 0)
			{
				c_emit_assign_var(c, vid, lt, right_val);
				if (IS_OPTIONAL(left_decl))
				{
					c_emit_assign_decl_fault(c, left_decl, right_val, NULL);
				}
			}
			else if (c_type_is_aggregate(lt) && (lt->type_kind == TYPE_ARRAY || lt->type_kind == TYPE_VECTOR || lt->type_kind == TYPE_SIMD_VECTOR))
			{
				int len = (int)lt->array.len;
				if (len <= 0)
				{
					len = 1;
				}
				bool r_is_agg = (right_val->type && c_type_is_aggregate(right_val->type));
				for (int k = 0; k < len; k++)
				{
					if (r_is_agg)
					{
						PRINTF("___var_%d.ptr[%d] %s ___var_%d.ptr[%d];\n", vid, k, assign_op, right_val->var, k);
					}
					else
					{
						PRINTF("___var_%d.ptr[%d] %s ___var_%d;\n", vid, k, assign_op, right_val->var);
					}
				}
			}
			else
			{
				if (type_is_pointer(lt) && lt->pointer && lt->pointer->type_kind == TYPE_VOID && (strcmp(assign_op, "+=") == 0 || strcmp(assign_op, "-=") == 0))
				{
					const char *arith_op = (strcmp(assign_op, "+=") == 0) ? "+" : "-";
					PRINTF("___var_%d = (void*)((char*)___var_%d %s ___var_%d);\n", vid, vid, arith_op, right_val ? right_val->var : 0);
				}
				else if (!right_val || right_val->var == 0)
				{
					PRINTF("___var_%d %s 0;\n", vid, assign_op);
				}
				else
				{
					PRINTF("___var_%d %s ___var_%d;\n", vid, assign_op, right_val->var);
				}
			}
			if (out_val)
			{
				if (strcmp(assign_op, "=") == 0 && right_val)
				{
					*out_val = *right_val;
				}
				else
				{
					*out_val = (CValue){.var = vid, .type = lt, .kind = CV_VALUE};
				}
			}
			return;
		}
	}

	Type *target_type = c_expr_type(left);
	if (target_type->type_kind == TYPE_VOID && right_val && right_val->type && is_valid_type_ptr(right_val->type))
	{
		target_type = c_safe_type_lower(right_val->type);
	}
	if (target_type->type_kind == TYPE_VOID)
	{
		target_type = type_int;
	}
	CValue addr_val = {0};
	c_emit_lvalue_addr(c, left, &addr_val, type_get_ptr(target_type));
	const char *tname = c_type_name(c, target_type);
	if (!right_val || right_val->var == 0)
	{
		if (c_type_is_aggregate(target_type))
		{
			PRINTF("__c3_memset((void*)___var_%d, 0, sizeof(%s));\n", addr_val.var, tname);
		}
		else if (type_is_pointer(target_type) || target_type->type_kind == TYPE_ANYFAULT || target_type->type_kind == TYPE_TYPEID)
		{
			PRINTF("*(%s*)___var_%d %s NULL;\n", tname, addr_val.var, assign_op);
		}
		else
		{
			PRINTF("*(%s*)___var_%d %s 0;\n", tname, addr_val.var, assign_op);
		}
		if (out_val)
		{
			int temp = c_emit_temp_var(c, out_val, target_type);
			c_emit_var_zero_init(c, temp, target_type);
		}
		return;
	}
	if (strcmp(assign_op, "=") == 0)
	{
		if (c_type_is_aggregate(target_type))
		{
			if (right_val->kind == CV_ADDRESS)
			{
				PRINTF("__c3_memcpy((void*)___var_%d, (void*)___var_%d, sizeof(%s));\n", addr_val.var, right_val->var, tname);
			}
			else
			{
				PRINTF("__c3_memcpy((void*)___var_%d, &___var_%d, sizeof(%s));\n", addr_val.var, right_val->var, tname);
			}
		}
		else if (target_type->type_kind == TYPE_POINTER && (right_val->type && (right_val->type->type_kind == TYPE_ANY || right_val->type->type_kind == TYPE_INTERFACE)))
		{
			PRINTF("*(%s*)___var_%d = (%s)___var_%d.ptr;\n", tname, addr_val.var, tname, right_val->var);
		}
		else if (target_type->type_kind == TYPE_POINTER && (right_val->type && right_val->type->type_kind == TYPE_SLICE))
		{
			PRINTF("*(%s*)___var_%d = (%s)___var_%d.ptr;\n", tname, addr_val.var, tname, right_val->var);
		}
		else if (target_type->type_kind == TYPE_POINTER && (right_val->type && (right_val->type->type_kind == TYPE_ARRAY || right_val->type->type_kind == TYPE_VECTOR || right_val->type->type_kind == TYPE_SIMD_VECTOR)))
		{
			PRINTF("*(%s*)___var_%d = (%s)(___var_%d%sptr);\n", tname, addr_val.var, tname, right_val->var, c_arrow(right_val));
		}
		else if (target_type->type_kind == TYPE_SLICE && (right_val->type && (right_val->type->type_kind == TYPE_ARRAY || right_val->type->type_kind == TYPE_VECTOR || right_val->type->type_kind == TYPE_SIMD_VECTOR)))
		{
			PRINTF("*(%s*)___var_%d = (%s){ .ptr = (void*)(___var_%d%sptr), .len = %llu };\n",
			       tname, addr_val.var, tname, right_val->var, c_arrow(right_val), (unsigned long long)right_val->type->array.len);
		}
		else if (target_type->type_kind == TYPE_ANY || target_type->type_kind == TYPE_INTERFACE)
		{
			char target_deref[64];
			snprintf(target_deref, sizeof(target_deref), "*(__c3_any__*)___var_%d", addr_val.var);
			c_emit_assign_to_any(c, target_deref, right_val);
		}
		else if (type_is_float(target_type) && (right_val->type && (type_is_pointer(right_val->type) || right_val->type->type_kind == TYPE_ANYFAULT || right_val->type->type_kind == TYPE_TYPEID)))
		{
			PRINTF("*(%s*)___var_%d = 0.0;\n", tname, addr_val.var);
		}
		else if ((type_is_pointer(target_type) || target_type->type_kind == TYPE_ANYFAULT || target_type->type_kind == TYPE_TYPEID) && type_is_float(right_val->type))
		{
			PRINTF("*(%s*)___var_%d = NULL;\n", tname, addr_val.var);
		}
		else if (target_type != right_val->type)
		{
			PRINTF("*(%s*)___var_%d = (%s)___var_%d;\n", tname, addr_val.var, tname, right_val->var);
		}
		else
		{
			PRINTF("*(%s*)___var_%d = ___var_%d;\n", tname, addr_val.var, right_val->var);
		}
	}
	else if (c_type_is_aggregate(target_type) && (target_type->type_kind == TYPE_ARRAY || target_type->type_kind == TYPE_VECTOR || target_type->type_kind == TYPE_SIMD_VECTOR))
	{
		int len = (int)target_type->array.len;
		if (len <= 0)
		{
			len = 1;
		}
		bool r_is_agg = (right_val->type && c_type_is_aggregate(right_val->type));
		for (int k = 0; k < len; k++)
		{
			if (r_is_agg)
			{
				PRINTF("((%s*)___var_%d)->ptr[%d] %s ___var_%d.ptr[%d];\n", tname, addr_val.var, k, assign_op, right_val->var, k);
			}
			else
			{
				PRINTF("((%s*)___var_%d)->ptr[%d] %s ___var_%d;\n", tname, addr_val.var, k, assign_op, right_val->var);
			}
		}
	}
	else
	{
		if (type_is_pointer(target_type) && target_type->pointer && target_type->pointer->type_kind == TYPE_VOID && (strcmp(assign_op, "+=") == 0 || strcmp(assign_op, "-=") == 0))
		{
			const char *arith_op = (strcmp(assign_op, "+=") == 0) ? "+" : "-";
			PRINTF("*(%s*)___var_%d = (void*)((char*)(*(%s*)___var_%d) %s ___var_%d);\n", tname, addr_val.var, tname, addr_val.var, arith_op, right_val ? right_val->var : 0);
		}
		else
		{
			PRINTF("*(%s*)___var_%d %s ___var_%d;\n", tname, addr_val.var, assign_op, right_val->var);
		}
	}
	if (out_val)
	{
		if (strcmp(assign_op, "=") == 0 && right_val)
		{
			*out_val = *right_val;
		}
		else
		{
			int res_temp = c_emit_temp_var(c, out_val, target_type);
			if (c_type_is_aggregate(target_type))
			{
				PRINTF("%s ___var_%d;\n", tname, res_temp);
				PRINTF("__c3_memcpy(&___var_%d, (void*)___var_%d, sizeof(%s));\n", res_temp, addr_val.var, tname);
			}
			else
			{
				PRINTF("%s ___var_%d = *(%s*)___var_%d;\n", tname, res_temp, tname, addr_val.var);
			}
		}
	}
}