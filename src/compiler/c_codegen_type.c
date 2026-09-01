#include "c_codegen_internal.h"

Decl *c_decl_unwrap(Decl *decl)
{
	if (!decl)
	{
		return NULL;
	}
	while (decl->replacement)
	{
		decl = decl->replacement;
	}
	while (decl->decl_kind == DECL_ALIAS)
	{
		decl = decl->define_decl.alias;
		while (decl && decl->replacement)
		{
			decl = decl->replacement;
		}
	}
	if (decl->decl_kind != DECL_VAR || decl->var.kind != VARDECL_UNWRAPPED)
	{
		return decl;
	}
	decl = decl->var.alias;
	while (decl && decl->replacement)
	{
		decl = decl->replacement;
	}
	return decl;
}

bool is_valid_type_ptr(Type *type)
{
	if (!type)
	{
		return false;
	}
	uintptr_t p = (uintptr_t)type;
	if (p < (uintptr_t)0x10000 || (p & 0x7) != 0)
	{
		return false;
	}
	if (type == poisoned_type)
	{
		return false;
	}
	if (type->type_kind < 0 || type->type_kind > TYPE_LAST)
	{
		return false;
	}
	return true;
}

Type *c_unwrap_alias(Type *type)
{
	int depth = 16;
	while (type && is_valid_type_ptr(type) && type->type_kind == TYPE_ALIAS &&
	       is_valid_type_ptr(type->canonical) && type->canonical != type && --depth > 0)
	{
		type = type->canonical;
	}
	return type;
}

Type *c_safe_type_lower(Type *type)
{
	if (!is_valid_type_ptr(type))
	{
		return type_void;
	}
	int limit = 64;
	while (type && is_valid_type_ptr(type) && limit-- > 0)
	{
		if (type->type_kind == TYPE_POISONED || type->type_kind == TYPE_WILDCARD || type->type_kind == TYPE_UNTYPEDLIST ||
		    type->type_kind == TYPE_TYPEINFO || type->type_kind == TYPE_MEMBER || type->type_kind == TYPE_REFLECTION)
		{
			return type_void;
		}
		if (type->type_kind == TYPE_ANYFAULT || type->type_kind == TYPE_TYPEID)
		{
			return type;
		}
		if (type->type_kind == TYPE_ALIAS)
		{
			if (is_valid_type_ptr(type->canonical) && type->canonical != type)
			{
				type = type->canonical;
				continue;
			}
			if (type->decl && type->decl->decl_kind == DECL_TYPE_ALIAS && type->decl->type_alias_decl.type_expr && is_valid_type_ptr(type->decl->type_alias_decl.type_expr->type))
			{
				type = type->decl->type_alias_decl.type_expr->type;
				continue;
			}
			return type_void;
		}
		if (type->type_kind == TYPE_OPTIONAL)
		{
			if (!is_valid_type_ptr(type->optional) || type->optional == type || type->optional->type_kind == TYPE_VOID)
			{
				return type_fault;
			}
			type = type->optional;
			continue;
		}
		if (type->type_kind == TYPE_TYPEDEF)
		{
			if (type->decl && type->decl->decl_kind == DECL_TYPEDEF && type->decl->distinct)
			{
				Type *dt = is_valid_type_ptr(type->decl->distinct->type) ? type->decl->distinct->type : NULL;
				if (dt && dt != type)
				{
					type = dt;
					continue;
				}
			}
			if (is_valid_type_ptr(type->canonical) && type->canonical != type)
			{
				type = type->canonical;
				continue;
			}
			break;
		}
		if (type->type_kind == TYPE_CONSTDEF || type->type_kind == TYPE_ENUM)
		{
			if (!type->decl || (type->decl->decl_kind != DECL_ENUM && type->decl->decl_kind != DECL_CONSTDEF) || !type->decl->enums.type_info)
			{
				return type_int;
			}
			Type *inner = enum_inner_type(type);
			if (!is_valid_type_ptr(inner) || inner == type)
			{
				return type_int;
			}
			type = inner;
			continue;
		}
		if (type->type_kind == TYPE_BITSTRUCT)
		{
			if (!type->decl || type->decl->decl_kind != DECL_BITSTRUCT || !type->decl->strukt.container_type || !is_valid_type_ptr(type->decl->strukt.container_type->type) || type->decl->strukt.container_type->type == type)
			{
				return type_uint;
			}
			type = type->decl->strukt.container_type->type;
			continue;
		}
		if (type->type_kind == TYPE_INTERFACE)
		{
			return type_any;
		}
		if (type->type_kind == TYPE_POINTER)
		{
			if (!is_valid_type_ptr(type->pointer) || type->pointer == type)
			{
				return type_voidptr;
			}
			Type *lowered_pointee = c_safe_type_lower(type->pointer);
			if (lowered_pointee == type->pointer)
			{
				return type;
			}
			return type_get_ptr(lowered_pointee);
		}
		if (type->type_kind == TYPE_SLICE)
		{
			if (!is_valid_type_ptr(type->array.base) || type->array.base == type)
			{
				return type_chars;
			}
			Type *lowered_base = c_safe_type_lower(type->array.base);
			if (lowered_base == type->array.base)
			{
				return type;
			}
			if (!type_is_valid_for_array(lowered_base))
			{
				return type_chars;
			}
			return type_get_slice(lowered_base);
		}
		if (type->type_kind == TYPE_ARRAY || type->type_kind == TYPE_VECTOR || type->type_kind == TYPE_SIMD_VECTOR || type->type_kind == TYPE_FLEXIBLE_ARRAY)
		{
			if (!is_valid_type_ptr(type->array.base) || type->array.base == type)
			{
				return type;
			}
			Type *lowered_base = c_safe_type_lower(type->array.base);
			if (lowered_base == type->array.base)
			{
				return type;
			}
			if (!type_is_valid_for_array(lowered_base))
			{
				return type;
			}
			if (type->type_kind == TYPE_ARRAY)
			{
				return type_get_array(lowered_base, type->array.len);
			}
			if (type->type_kind == TYPE_FLEXIBLE_ARRAY)
			{
				return type_get_flexible_array(lowered_base);
			}
			return type_get_vector_from_vector(lowered_base, type);
		}
		if (type->type_kind == TYPE_FUNC_PTR)
		{
			return type;
		}
		if (is_valid_type_ptr(type->canonical) && type->canonical != type)
		{
			type = type->canonical;
			continue;
		}
		break;
	}
	if (!is_valid_type_ptr(type) || limit <= 0)
	{
		return type_void;
	}
	return type;
}

Type *c_decl_type(Decl *decl)
{
	if (!decl)
	{
		return type_void;
	}
	decl = c_decl_unwrap(decl);
	if (decl->type && is_valid_type_ptr(decl->type) && decl->type->type_kind != TYPE_POISONED)
	{
		return c_safe_type_lower(decl->type);
	}
	if (decl->decl_kind == DECL_VAR)
	{
		if (decl->var.type_info)
		{
			TypeInfo *ti = type_infoptrzero(decl->var.type_info);
			if (ti)
			{
				if (ti->type && is_valid_type_ptr(ti->type))
				{
					return c_safe_type_lower(ti->type);
				}
				if (ti->kind == TYPE_INFO_IDENTIFIER && ti->unresolved.name)
				{
					Decl *td = NULL;
					if (decl->unit && decl->unit->module)
					{
						td = htable_get(&decl->unit->module->symbols, (void *)ti->unresolved.name);
					}
					if (!td && compiler.context.symbols.entries)
					{
						DeclId id = decltable_get(&compiler.context.symbols, ti->unresolved.name);
						if (id)
						{
							td = declptr(id);
						}
					}
					if (td)
					{
						td = c_decl_unwrap(td);
						if (td->type && is_valid_type_ptr(td->type))
						{
							return c_safe_type_lower(td->type);
						}
					}
				}
			}
		}
		if (decl->var.init_expr)
		{
			Type *t = c_expr_type(decl->var.init_expr);
			if (t && is_valid_type_ptr(t) && t->type_kind != TYPE_VOID)
			{
				return t;
			}
		}
	}
	if (decl->decl_kind == DECL_FUNC && decl->func_decl.signature.rtype)
	{
		Type *t = typeget(decl->func_decl.signature.rtype);
		if (t && is_valid_type_ptr(t))
		{
			return c_safe_type_lower(t);
		}
	}
	return type_void;
}

Type *c_expr_type(Expr *expr)
{
	if (!expr)
	{
		return type_void;
	}
	if (expr->type && is_valid_type_ptr(expr->type) && expr->type->type_kind != TYPE_POISONED)
	{
		return c_safe_type_lower(expr->type);
	}
	if (expr->expr_kind == EXPR_IDENTIFIER && expr->ident_expr)
	{
		return c_decl_type(expr->ident_expr);
	}
	if (expr->expr_kind == EXPR_ACCESS_RESOLVED && expr->access_resolved_expr.ref)
	{
		return c_decl_type(expr->access_resolved_expr.ref);
	}
	if (expr->expr_kind == EXPR_DECL && expr->decl_expr)
	{
		return c_decl_type(expr->decl_expr);
	}
	if (expr->expr_kind == EXPR_UNARY)
	{
		if (expr->unary_expr.operator == UNARYOP_ADDR || expr->unary_expr.operator == UNARYOP_TADDR)
		{
			Type *inner_t = c_expr_type(expr->unary_expr.expr);
			if (inner_t->type_kind != TYPE_VOID)
			{
				return type_get_ptr(inner_t);
			}
		}
		else if (expr->unary_expr.operator == UNARYOP_DEREF)
		{
			Type *inner_t = c_expr_type(expr->unary_expr.expr);
			if (inner_t->type_kind == TYPE_POINTER && inner_t->pointer)
			{
				return c_safe_type_lower(inner_t->pointer);
			}
		}
		else if (expr->unary_expr.operator == UNARYOP_NOT)
		{
			return type_bool;
		}
		else
		{
			return c_expr_type(expr->unary_expr.expr);
		}
	}
	if (expr->expr_kind == EXPR_SUBSCRIPT || expr->expr_kind == EXPR_SUBSCRIPT_ADDR)
	{
		Type *parent_t = c_expr_type(exprptrzero(expr->subscript_expr.expr));
		if (parent_t->type_kind == TYPE_POINTER && parent_t->pointer)
		{
			return c_safe_type_lower(parent_t->pointer);
		}
		if ((parent_t->type_kind == TYPE_SLICE || parent_t->type_kind == TYPE_ARRAY || parent_t->type_kind == TYPE_VECTOR || parent_t->type_kind == TYPE_SIMD_VECTOR) && parent_t->array.base)
		{
			return c_safe_type_lower(parent_t->array.base);
		}
	}
	if (expr->expr_kind == EXPR_SLICE)
	{
		Type *parent_t = c_expr_type(exprptrzero(expr->slice_expr.expr));
		if (parent_t->type_kind == TYPE_SLICE)
		{
			return parent_t;
		}
		if (parent_t->type_kind == TYPE_ARRAY && parent_t->array.base)
		{
			return type_get_slice(parent_t->array.base);
		}
		if (parent_t->type_kind == TYPE_POINTER && parent_t->pointer)
		{
			return type_get_slice(parent_t->pointer);
		}
		return type_chars;
	}
	if (expr->expr_kind == EXPR_BINARY)
	{
		if (expr->binary_expr.operator >= BINARYOP_GT && expr->binary_expr.operator <= BINARYOP_EQ)
		{
			return type_bool;
		}
		if (expr->binary_expr.operator == BINARYOP_AND || expr->binary_expr.operator == BINARYOP_OR)
		{
			return type_bool;
		}
		return c_expr_type(exprptrzero(expr->binary_expr.left));
	}
	return type_void;
}

bool c_type_is_aggregate(Type *t)
{
	if (!t || !is_valid_type_ptr(t))
	{
		return false;
	}
	t = c_safe_type_lower(t);
	return t->type_kind == TYPE_STRUCT || t->type_kind == TYPE_UNION ||
	       t->type_kind == TYPE_ARRAY || t->type_kind == TYPE_SLICE ||
	       t->type_kind == TYPE_VECTOR || t->type_kind == TYPE_SIMD_VECTOR ||
	       t->type_kind == TYPE_FLEXIBLE_ARRAY ||
	       t->type_kind == TYPE_ANY || t->type_kind == TYPE_INTERFACE;
}

bool c_type_is_resolved(Type *type)
{
	type = c_unwrap_alias(type);
	if (!type || !is_valid_type_ptr(type))
	{
		return false;
	}
	switch (type->type_kind)
	{
		case TYPE_STRUCT:
		case TYPE_UNION:
		case TYPE_BITSTRUCT:
		case TYPE_ENUM:
		case TYPE_CONSTDEF:
		case TYPE_TYPEDEF:
		case TYPE_INTERFACE:
			if (!type->decl || type->decl->is_template || type->decl->resolve_status != RESOLVE_DONE)
			{
				return false;
			}
			return true;
		case TYPE_POINTER:
			return c_type_is_resolved(type->pointer);
		case TYPE_SLICE:
		case TYPE_ARRAY:
		case TYPE_VECTOR:
		case TYPE_SIMD_VECTOR:
		case TYPE_FLEXIBLE_ARRAY:
			return c_type_is_resolved(type->array.base);
		case TYPE_OPTIONAL:
			if (!type->optional || type->optional->type_kind == TYPE_VOID || type->optional->type_kind == TYPE_WILDCARD)
			{
				return false;
			}
			return c_type_is_resolved(type->optional);
		case TYPE_FUNC_PTR:
			return true;
		case TYPE_VOID:
		case TYPE_BOOL:
		case ALL_INTS:
		case ALL_FLOATS:
		case TYPE_ANY:
		case TYPE_ANYFAULT:
		case TYPE_TYPEID:
			return true;
		default:
			return false;
	}
}

int c_get_type_introspection_kind(Type *type)
{
	type = c_unwrap_alias(type);
	if (!type || !is_valid_type_ptr(type))
	{
		return INTROSPECT_TYPE_VOID;
	}
	switch (type->type_kind)
	{
		case TYPE_VOID: return INTROSPECT_TYPE_VOID;
		case TYPE_BOOL: return INTROSPECT_TYPE_BOOL;
		case TYPE_I8:
		case TYPE_I16:
		case TYPE_I32:
		case TYPE_I64:
		case TYPE_I128: return INTROSPECT_TYPE_SIGNED_INT;
		case TYPE_U8:
		case TYPE_U16:
		case TYPE_U32:
		case TYPE_U64:
		case TYPE_U128: return INTROSPECT_TYPE_UNSIGNED_INT;
		case TYPE_F16:
		case TYPE_BF16:
		case TYPE_F32:
		case TYPE_F64:
		case TYPE_F128: return INTROSPECT_TYPE_FLOAT;
		case TYPE_TYPEID: return INTROSPECT_TYPE_TYPEID;
		case TYPE_ANYFAULT: return INTROSPECT_TYPE_ANYFAULT;
		case TYPE_ANY: return INTROSPECT_TYPE_ANY;
		case TYPE_ENUM: return INTROSPECT_TYPE_ENUM;
		case TYPE_CONSTDEF: return INTROSPECT_TYPE_CONSTDEF;
		case TYPE_STRUCT: return INTROSPECT_TYPE_STRUCT;
		case TYPE_UNION: return INTROSPECT_TYPE_UNION;
		case TYPE_BITSTRUCT: return INTROSPECT_TYPE_BITSTRUCT;
		case TYPE_FUNC_PTR:
		case TYPE_FUNC_RAW: return INTROSPECT_TYPE_FUNC;
		case TYPE_OPTIONAL: return INTROSPECT_TYPE_OPTIONAL;
		case TYPE_ARRAY: return INTROSPECT_TYPE_ARRAY;
		case TYPE_SLICE: return INTROSPECT_TYPE_SLICE;
		case TYPE_VECTOR:
		case TYPE_SIMD_VECTOR: return INTROSPECT_TYPE_VECTOR;
		case TYPE_TYPEDEF: return INTROSPECT_TYPE_DISTINCT;
		case TYPE_POINTER: return INTROSPECT_TYPE_POINTER;
		case TYPE_INTERFACE: return INTROSPECT_TYPE_INTERFACE;
		default: return INTROSPECT_TYPE_VOID;
	}
}

size_t c_get_type_size(Type *type)
{
	type = c_unwrap_alias(type);
	if (!type || !is_valid_type_ptr(type) || !c_type_is_resolved(type))
	{
		return 0;
	}
	switch (type->type_kind)
	{
		case TYPE_VOID:
		case TYPE_FLEXIBLE_ARRAY:
		case TYPE_FUNC_RAW:
		case TYPE_POISONED:
		case TYPE_WILDCARD:
		case TYPE_UNTYPEDLIST:
		case TYPE_TYPEINFO:
		case TYPE_MEMBER:
		case TYPE_REFLECTION:
		case TYPE_INFERRED_ARRAY:
		case TYPE_INFERRED_VECTOR:
			return 0;
		default:
			return (size_t)type_size(type);
	}
}

const char *c_type_name(GenContext *c, Type *type)
{
	if (!type || !is_valid_type_ptr(type))
	{
		return "void";
	}
	type = c_safe_type_lower(type);
	switch (type->type_kind)
	{
		case TYPE_VOID: return "void";
		case TYPE_BOOL: return "bool";
		case TYPE_I8: return "int8_t";
		case TYPE_I16: return "int16_t";
		case TYPE_I32: return "int32_t";
		case TYPE_I64: return "int64_t";
		case TYPE_I128: return "__c3_int128";
		case TYPE_U8: return "uint8_t";
		case TYPE_U16: return "uint16_t";
		case TYPE_U32: return "uint32_t";
		case TYPE_U64: return "uint64_t";
		case TYPE_U128: return "__c3_uint128";
		case TYPE_F16:
		case TYPE_BF16:
		case TYPE_F32: return "float";
		case TYPE_F64:
		case TYPE_F128: return "double";
		case TYPE_ANYFAULT: return "c3fault_t";
		case TYPE_TYPEID: return "c3typeid_t";
		case TYPE_ANY:
		case TYPE_INTERFACE: return "__c3_any__";
		case TYPE_TYPEDEF:
			if (type->decl && type->decl->distinct && is_valid_type_ptr(type->decl->distinct->type))
			{
				return c_type_name(c, type->decl->distinct->type);
			}
			return "void*";
		case TYPE_POINTER: return c_intern(str_printf("%s*", c_type_name(c, type->pointer)));
		case TYPE_STRUCT:
		case TYPE_UNION:
		{
			Decl *d = type->decl;
			return d ? c_get_decl_name(d) : c_intern(str_printf("__c3_struct_%" PRIxPTR, (uintptr_t)type));
		}
		case TYPE_BITSTRUCT:
			return c_type_name(c, type->decl->strukt.container_type->type);
		case TYPE_SLICE:
		{
			const char *base = c_type_name(c, type->array.base);
			return c_intern(str_printf("__c3_slice_%s", c_sanitize_name(base)));
		}
		case TYPE_ARRAY:
		case TYPE_FLEXIBLE_ARRAY:
		case TYPE_VECTOR:
		case TYPE_SIMD_VECTOR:
		{
			const char *base       = c_type_name(c, type->array.base);
			unsigned long long len = type->type_kind == TYPE_FLEXIBLE_ARRAY ? 0 : (unsigned long long)type->array.len;
			if (len == 0 && type->type_kind != TYPE_FLEXIBLE_ARRAY)
			{
				len = 1;
			}
			return c_intern(str_printf("__c3_array_%s_%llu", c_sanitize_name(base), len));
		}
		case TYPE_FUNC_PTR: return c_intern(str_printf("__c3_fnptr_%" PRIxPTR, (uintptr_t)type));
		default: return "void*";
	}
}

const char *c_type_zero_literal(Type *t)
{
	if (!t || !is_valid_type_ptr(t))
	{
		return "0";
	}
	t = c_safe_type_lower(t);
	if (c_type_is_aggregate(t))
	{
		return "{0}";
	}
	if (type_is_pointer(t) || t->type_kind == TYPE_FUNC_PTR ||
	    t->type_kind == TYPE_ANYFAULT || t->type_kind == TYPE_TYPEID)
	{
		return "NULL";
	}
	if (type_is_float(t))
	{
		return "0.0";
	}
	return "0";
}

static bool c_emit_struct_or_union_forward_decl(GenContext *c, Type *type, const char *keyword)
{
	const char *name = c_type_name(c, type);
	if (htable_get(&c->gen_decl, (void *)name))
	{
		return false;
	}
	htable_set(&c->gen_decl, (void *)name, (void *)1);
	Decl *d = type->decl;
	PRINTF("typedef %s %s__ %s;\n", keyword, name, name);
	if (d && d->strukt.members)
	{
		FOREACH(Decl *, m, d->strukt.members)
		{
			if (m && is_valid_type_ptr(m->type))
			{
				c_emit_type_forward_decl(c, m->type);
			}
		}
	}
	return true;
}

static bool c_emit_struct_or_union_decl(GenContext *c, Type *type, const char *keyword)
{
	const char *name = c_type_name(c, type);
	if (htable_get(&c->gen_def, (void *)name))
	{
		return false;
	}
	htable_set(&c->gen_def, (void *)name, (void *)1);

	Decl *d         = type->decl;
	int field_count = 0;
	if (d && d->strukt.members)
	{
		FOREACH(Decl *, m, d->strukt.members)
		{
			if (!m || !is_valid_type_ptr(m->type))
			{
				continue;
			}
			Type *mt = c_safe_type_lower(m->type);
			if (!mt || mt->type_kind == TYPE_VOID)
			{
				continue;
			}
			if (mt->type_kind != TYPE_POINTER)
			{
				c_emit_type_decl(c, mt);
			}
			field_count++;
		}
	}

	PRINTF("%s %s__ {\n", keyword, name);
	if (field_count == 0)
	{
		PRINT("\tchar __empty;\n");
	}
	else
	{
		FOREACH_IDX(i, Decl *, m, d->strukt.members)
		{
			if (!m || !is_valid_type_ptr(m->type))
			{
				continue;
			}
			Type *mt = c_safe_type_lower(m->type);
			if (!mt || mt->type_kind == TYPE_VOID)
			{
				mt = type_char;
			}
			if (m->alignment && m->alignment > type_abi_alignment(mt))
			{
				PRINTF("\t%s m%d __c3_aligned(%u);\n", c_type_name(c, mt), (int)i, m->alignment);
			}
			else
			{
				PRINTF("\t%s m%d;\n", c_type_name(c, mt), (int)i);
			}
		}
	}
	AlignSize align = d ? d->alignment : 0;
	bool packed     = d ? d->strukt.is_packed : false;
	if (align || packed)
	{
		PRINT("} ");
		if (packed)
		{
			PRINT("__c3_packed ");
		}
		if (align)
		{
			PRINTF("__c3_aligned(%u) ", align);
		}
		PRINT(";\n");
	}
	else
	{
		PRINT("};\n");
	}
	return true;
}

bool c_emit_type_forward_decl(GenContext *c, Type *type)
{
	if (!type || !is_valid_type_ptr(type))
	{
		return false;
	}
	type = c_safe_type_lower(type);
	if (!type || !is_valid_type_ptr(type))
	{
		return false;
	}

	if (type == type_u128 || type == type_i128)
	{
		htable_set(&c->gen_decl, (void *)c_type_name(c, type), (void *)1);
		return true;
	}
	switch (type->type_kind)
	{
		case TYPE_POINTER:
			return c_emit_type_forward_decl(c, type->pointer);
		case TYPE_STRUCT:
			return c_emit_struct_or_union_forward_decl(c, type, "struct");
		case TYPE_UNION:
			return c_emit_struct_or_union_forward_decl(c, type, "union");
		case TYPE_SLICE:
		{
			const char *slice_name = c_type_name(c, type);
			if (htable_get(&c->gen_decl, (void *)slice_name))
			{
				return false;
			}
			htable_set(&c->gen_decl, (void *)slice_name, (void *)1);
			c_emit_type_forward_decl(c, type->array.base);
			const char *base_tname = c_type_name(c, type->array.base);
			PRINTF("#ifndef %s_defined__\n#define %s_defined__\n", slice_name, slice_name);
			PRINTF("typedef struct { %s* ptr; size_t len; } %s;\n", base_tname, slice_name);
			PRINT("#endif\n");
			return true;
		}
		case TYPE_ARRAY:
		case TYPE_FLEXIBLE_ARRAY:
		case TYPE_SIMD_VECTOR:
		case TYPE_VECTOR:
		{
			const char *arr_name = c_type_name(c, type);
			if (htable_get(&c->gen_decl, (void *)arr_name))
			{
				return false;
			}
			htable_set(&c->gen_decl, (void *)arr_name, (void *)1);
			c_emit_type_forward_decl(c, type->array.base);
			PRINTF("#ifndef %s_defined__\n#define %s_defined__\n", arr_name, arr_name);
			PRINTF("typedef struct %s__ %s;\n", arr_name, arr_name);
			PRINT("#endif\n");
			return true;
		}
		case TYPE_FUNC_PTR:
		{
			const char *fnptr_name = c_type_name(c, type);
			if (htable_get(&c->gen_decl, (void *)fnptr_name))
			{
				return false;
			}
			htable_set(&c->gen_decl, (void *)fnptr_name, (void *)1);

			Type *raw_func = type->pointer;
			Signature *sig = (raw_func && is_valid_type_ptr(raw_func) && raw_func->type_kind == TYPE_FUNC_RAW) ? raw_func->function.signature : NULL;
			Type *rtype    = (sig && sig->rtype) ? typeget(sig->rtype) : type_void;
			if (is_valid_type_ptr(rtype))
			{
				c_emit_type_forward_decl(c, rtype);
			}
			if (sig && sig->params)
			{
				FOREACH(Decl *, p, sig->params)
				{
					if (p && is_valid_type_ptr(p->type))
					{
						c_emit_type_forward_decl(c, p->type);
					}
				}
			}

			PRINTF("#ifndef %s_defined__\n#define %s_defined__\n", fnptr_name, fnptr_name);
			PRINTF("typedef %s (*%s)(", c_type_name(c, rtype), fnptr_name);
			int param_count = sig ? vec_size(sig->params) : 0;
			if (param_count == 0 && (!sig || sig->variadic == VARIADIC_NONE))
			{
				PRINT("void");
			}
			else
			{
				FOREACH_IDX(i, Decl *, p, sig->params)
				{
					if (i > 0)
					{
						PRINT(", ");
					}
					PRINT(c_type_name(c, (p && is_valid_type_ptr(p->type)) ? p->type : type_void));
				}
				if (sig && sig->variadic == VARIADIC_RAW)
				{
					if (param_count > 0)
					{
						PRINT(", ");
					}
					PRINT("...");
				}
			}
			PRINT(");\n#endif\n");
			return true;
		}
		default:
			return false;
	}
}

bool c_emit_type_decl(GenContext *c, Type *type)
{
	if (!type || !is_valid_type_ptr(type))
	{
		return false;
	}
	type = c_safe_type_lower(type);
	if (!type || !is_valid_type_ptr(type))
	{
		return false;
	}

	switch (type->type_kind)
	{
		case TYPE_STRUCT:
			return c_emit_struct_or_union_decl(c, type, "struct");
		case TYPE_UNION:
			return c_emit_struct_or_union_decl(c, type, "union");
		case TYPE_ARRAY:
		case TYPE_FLEXIBLE_ARRAY:
		case TYPE_SIMD_VECTOR:
		case TYPE_VECTOR:
		{
			const char *arr_name = c_type_name(c, type);
			if (htable_get(&c->gen_def, (void *)arr_name))
			{
				return false;
			}
			htable_set(&c->gen_def, (void *)arr_name, (void *)1);

			c_emit_type_decl(c, type->array.base);
			const char *base_tname = c_type_name(c, type->array.base);
			unsigned long long len = type->type_kind == TYPE_FLEXIBLE_ARRAY ? 0 : (unsigned long long)type->array.len;
			if (len == 0 && type->type_kind != TYPE_FLEXIBLE_ARRAY)
			{
				len = 1;
			}
			AlignSize vec_align = (type->type_kind == TYPE_SIMD_VECTOR) ? type_abi_alignment(type) : 0;
			const char *ext     = (len == 0) ? "__extension__ " : "";
			if (vec_align > type_abi_alignment(type->array.base))
			{
				PRINTF("%sstruct %s__ { %s ptr[%llu]; } __c3_aligned(%u);\n", ext, arr_name, base_tname, len, vec_align);
			}
			else
			{
				PRINTF("%sstruct %s__ { %s ptr[%llu]; };\n", ext, arr_name, base_tname, len);
			}
			return true;
		}
		case TYPE_BITSTRUCT:
		{
			Decl *d = type->decl;
			if (!d)
			{
				return false;
			}
			const char *bs_name = c_get_decl_name(d);
			if (htable_get(&c->gen_def, (void *)bs_name))
			{
				return false;
			}
			htable_set(&c->gen_def, (void *)bs_name, (void *)1);
			return true;
		}
		default:
			return false;
	}
}

static const char *c_typeid_mangled(Type *type)
{
	type = c_unwrap_alias(type);
	if (!type || !is_valid_type_ptr(type))
	{
		return "void";
	}
	switch (type->type_kind)
	{
		case TYPE_VOID: return "void";
		case TYPE_BOOL: return "bool";
		case TYPE_I8: return "int8";
		case TYPE_I16: return "int16";
		case TYPE_I32: return "int32";
		case TYPE_I64: return "int64";
		case TYPE_I128: return "int128";
		case TYPE_U8: return "uint8";
		case TYPE_U16: return "uint16";
		case TYPE_U32: return "uint32";
		case TYPE_U64: return "uint64";
		case TYPE_U128: return "uint128";
		case TYPE_F16: return "float16";
		case TYPE_BF16: return "bfloat16";
		case TYPE_F32: return "float";
		case TYPE_F64: return "double";
		case TYPE_F128: return "float128";
		case TYPE_ANYFAULT: return "fault";
		case TYPE_TYPEID: return "typeid";
		case TYPE_ANY: return "any";
		case TYPE_INTERFACE:
		case TYPE_STRUCT:
		case TYPE_UNION:
		case TYPE_BITSTRUCT:
		case TYPE_ENUM:
		case TYPE_CONSTDEF:
		case TYPE_TYPEDEF:
			if (type->decl)
			{
				return c_get_decl_name(type->decl);
			}
			return str_printf("anon_%" PRIxPTR, (uintptr_t)type);
		case TYPE_POINTER:
			return str_printf("p_%s", c_typeid_mangled(type->pointer));
		case TYPE_SLICE:
			return str_printf("slice_%s", c_typeid_mangled(type->array.base));
		case TYPE_FLEXIBLE_ARRAY:
			return str_printf("flex_%s", c_typeid_mangled(type->array.base));
		case TYPE_ARRAY:
			return str_printf("arr_%llu_%s", (unsigned long long)type->array.len, c_typeid_mangled(type->array.base));
		case TYPE_VECTOR:
			return str_printf("vec_%llu_%s", (unsigned long long)type->array.len, c_typeid_mangled(type->array.base));
		case TYPE_SIMD_VECTOR:
			return str_printf("svec_%llu_%s", (unsigned long long)type->array.len, c_typeid_mangled(type->array.base));
		case TYPE_OPTIONAL:
			if (!type->optional || type->optional->type_kind == TYPE_VOID || type->optional->type_kind == TYPE_WILDCARD)
			{
				return "opt_void";
			}
			return str_printf("opt_%s", c_typeid_mangled(type->optional));
		case TYPE_FUNC_PTR:
		case TYPE_FUNC_RAW:
			return str_printf("fn_%" PRIxPTR, (uintptr_t)type);
		default:
			return "void";
	}
}

const char *c_typeid_name(Type *type)
{
	type = c_unwrap_alias(type);
	if (!type || !is_valid_type_ptr(type))
	{
		return "__c3_typeid_void";
	}
	const char *mangled = c_typeid_mangled(type);
	return c_intern(str_printf("__c3_typeid_%s", c_sanitize_name(mangled)));
}

const char *c_fault_symbol_name(Decl *decl)
{
	if (!decl)
	{
		return "NULL";
	}
	const char *name = c_get_decl_name(decl);
	return c_intern(str_printf("__c3_fault_%s", name));
}

Type *c_get_method_target_type(Decl *m)
{
	if (!m || m->decl_kind != DECL_FUNC)
	{
		return NULL;
	}
	if (m->func_decl.type_parent)
	{
		TypeInfo *ti = type_infoptr(m->func_decl.type_parent);
		if (ti && is_valid_type_ptr(ti->type))
		{
			Type *t = ti->type;
			while (t && is_valid_type_ptr(t) && t->type_kind == TYPE_ALIAS && t->canonical != t)
			{
				t = t->canonical;
			}
			if (t && type_is_pointer(t) && t->pointer)
			{
				t = t->pointer;
			}
			while (t && is_valid_type_ptr(t) && t->type_kind == TYPE_ALIAS && t->canonical != t)
			{
				t = t->canonical;
			}
			return t;
		}
	}
	Signature *sig = &m->func_decl.signature;
	if (sig->params && vec_size(sig->params) > 0)
	{
		Decl *self_p = sig->params[0];
		if (self_p && (self_p->var.is_self || (self_p->name && strcmp(self_p->name, "self") == 0)))
		{
			Type *pt = self_p->type ? self_p->type : c_decl_type(self_p);
			if (pt && is_valid_type_ptr(pt))
			{
				while (pt && is_valid_type_ptr(pt) && pt->type_kind == TYPE_ALIAS && pt->canonical != pt)
				{
					pt = pt->canonical;
				}
				if (type_is_pointer(pt) && pt->pointer)
				{
					pt = pt->pointer;
				}
				while (pt && is_valid_type_ptr(pt) && pt->type_kind == TYPE_ALIAS && pt->canonical != pt)
				{
					pt = pt->canonical;
				}
				return pt;
			}
		}
	}
	return NULL;
}