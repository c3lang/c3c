#ifndef C_CODEGEN_INTERNAL_H
#define C_CODEGEN_INTERNAL_H

#include "codegen_internal.h"
#include <inttypes.h>

#include "../utils/byte_buffer.h"

static inline uint16_t c_bswap16(uint16_t x)
{
#if defined(_MSC_VER)
	return _byteswap_ushort(x);
#elif defined(__GNUC__) || defined(__clang__)
	return __builtin_bswap16(x);
#else
	return (uint16_t)((x << 8) | (x >> 8));
#endif
}

static inline uint32_t c_bswap32(uint32_t x)
{
#if defined(_MSC_VER)
	return _byteswap_ulong(x);
#elif defined(__GNUC__) || defined(__clang__)
	return __builtin_bswap32(x);
#else
	return ((x << 24) & 0xff000000u) |
	       ((x << 8) & 0x00ff0000u) |
	       ((x >> 8) & 0x0000ff00u) |
	       ((x >> 24) & 0x000000ffu);
#endif
}

static inline uint64_t c_bswap64(uint64_t x)
{
#if defined(_MSC_VER)
	return _byteswap_uint64(x);
#elif defined(__GNUC__) || defined(__clang__)
	return __builtin_bswap64(x);
#else
	return ((x << 56) & 0xff00000000000000ull) |
	       ((x << 40) & 0x00ff000000000000ull) |
	       ((x << 24) & 0x0000ff0000000000ull) |
	       ((x << 8) & 0x000000ff00000000ull) |
	       ((x >> 8) & 0x00000000ff000000ull) |
	       ((x >> 24) & 0x0000000000ff0000ull) |
	       ((x >> 40) & 0x000000000000ff00ull) |
	       ((x >> 56) & 0x00000000000000ffull);
#endif
}

#define PRINT(s) c_print(c, (s))
#define PRINTF(...) c_printf(c, __VA_ARGS__)

typedef int VariableId;

typedef enum
{
	CV_VALUE,
	CV_ADDRESS
} CValueKind;

typedef struct
{
	VariableId var;
	Type *type;
	CValueKind kind;
	int optional;
} CValue;

typedef struct
{
	VariableId id;
	Type *type;
	bool is_static;
	Decl *decl;
} CLocalVar;

typedef struct GenContext
{
	FILE *file;
	ByteBuffer *buffer;
	const char *base_name;
	const char *c_filename;
	const char *object_filename;
	Module *current_module;
	int id_gen;
	bool current_block_live;

	Type *current_return_type;
	int current_macro_ret_var;
	int current_macro_fault_var;
	Type *current_macro_ret_type;
	int current_macro_exit_label;
	int current_break_label;
	int current_continue_label;
	CValue retval;

	CLocalVar *function_locals;

	HTable gen_decl;
	HTable gen_def;
	HTable local_vars;
	HTable local_fault_vars;
	HTable declared_vars;
	HTable decl_names;
	HTable emitted_func_defs;
	HTable emitted_global_decls;
	HTable emitted_global_defs;
} GenContext;

typedef void (*CDeclVisitor)(GenContext *c, Decl *d, void *userdata);

/* Output */
void c_write_bytes(GenContext *c, const uint8_t *bytes, size_t len);
void c_print(GenContext *c, const char *str);
void c_printf(GenContext *c, const char *fmt, ...);
void c_print_byte(GenContext *c, uint8_t b);

/* Module & Utility */
const char *c_intern(const char *str);
const char *c_sanitize_name(const char *name);
const char *c_get_decl_name(Decl *decl);
const char *c_get_decl_asm_name(Decl *decl);
const char *c_get_enum_assoc_name(Decl *enum_decl, Decl *param);
void c_emit_string_literal(GenContext *c, const char *bytes, ArrayIndex len);
void c_traverse_all_modules(GenContext *c, CDeclVisitor visitor, void *userdata);

/* Function & Variable Management */
int c_create_label(GenContext *c);
int c_create_variable(GenContext *c);
void c_register_function_local(GenContext *c, VariableId id, Type *type, bool is_static, Decl *decl);
VariableId c_emit_temp_var(GenContext *c, CValue *value, Type *type);
VariableId c_get_or_create_decl_var(GenContext *c, Decl *decl);
VariableId c_get_decl_fault_var(GenContext *c, Decl *decl);
void c_emit_assign_decl_fault(GenContext *c, Decl *decl, CValue *src_val, Expr *src_expr);
VariableId c_ensure_cvalue_var(GenContext *c, CValue *val, Type *fallback_type);
void c_value_rvalue(GenContext *c, CValue *val);
void c_value_addr(GenContext *c, CValue *val);
const char *c_arrow(const CValue *val);
void c_emit_label(GenContext *c, int label_id);
void c_emit_var_zero_init(GenContext *c, VariableId var_id, Type *type);
void c_emit_assign_to_any(GenContext *c, const char *dst_expr, CValue *src);
void c_emit_assign_var(GenContext *c, VariableId dst_var, Type *dst_type, CValue *src_val);
bool c_emit_function_decl(GenContext *c, Decl *fn, bool is_current_module);
void c_emit_function(GenContext *c, Decl *fn);
void c_emit_local_var_declarations(GenContext *c);

/* Lvalue & Access */
Decl *get_struct_decl_from_type(Type *type);
bool c_find_member_index_rec(Decl *parent_decl, Decl *member, int *out_indices, int *out_depth, int max_depth);
bool c_is_bitstruct_array(Type *type, Decl **out_decl);
bool c_is_bitstruct_big_endian(Decl *d);
bool c_is_bitstruct_requires_byteswap(Decl *d);
void c_get_bitstruct_member_bits(Decl *m, int *out_start_bit, int *out_end_bit);
void c_emit_bitstruct_accessors_to_file(FILE *f, Decl *decl);
const char *c_emit_bitstruct_container(GenContext *c, Expr *parent_expr, Decl *member, int *out_container_var, Decl **out_bitstruct_decl);
void c_emit_lvalue_addr(GenContext *c, Expr *expr, CValue *out_val, Type *target_type);
void c_emit_lvalue_read(GenContext *c, Expr *expr, CValue *out_val, Type *target_type);
void c_emit_lvalue_assign(GenContext *c, Expr *left, CValue *right_val, const char *assign_op, CValue *out_val);

/* Global & Runtime */
bool c_is_file_global(Decl *decl);
bool c_can_emit_static_initializer(Expr *expr);
void c_emit_const_initializer_rec(GenContext *c, ConstInitializer *init);
void c_emit_const_init_expr(GenContext *c, Expr *expr, Type *type);
void c_emit_global_decl(GenContext *c, Decl *var);
void c_emit_global_def(GenContext *c, Decl *var);
void c_emit_dynamic_dispatcher(GenContext *c, Decl *dyn_fn);
void c_emit_runtime_header(const char *dir);
GenContext *c_emit_runtime_c(const char *dir);
void c_emit_const_float_literal(GenContext *c, double f);
void c_emit_const_int_literal(GenContext *c, const ExprConst *ec, Type *type);

typedef struct
{
	int bit;
	int byte_idx;
	int bit_in_byte;
	int step_bits;
	int shift;
	uint32_t step_mask;
	uint32_t byte_mask;
} CBitfieldStep;

static inline CBitfieldStep c_bitfield_step(int start_bit, int bit_size, int bits_done, bool reverse)
{
	CBitfieldStep s;
	s.bit          = start_bit + bits_done;
	s.byte_idx     = s.bit / 8;
	s.bit_in_byte  = s.bit % 8;
	int rem_byte   = 8 - s.bit_in_byte;
	int rem_bits   = bit_size - bits_done;
	s.step_bits    = rem_bits < rem_byte ? rem_bits : rem_byte;
	s.shift        = reverse ? (bit_size - bits_done - s.step_bits) : bits_done;
	s.step_mask    = (1U << s.step_bits) - 1;
	s.byte_mask    = s.step_mask << s.bit_in_byte;
	return s;
}

static inline int c_type_aggregate_len(Type *t)
{
	if (!t)
	{
		return 1;
	}
	t = type_flatten(t);
	if (!t)
	{
		return 1;
	}
	int len = (int)t->array.len;
	return len > 0 ? len : 1;
}

static inline bool c_decl_vec_contains(Decl **vec, Decl *target)
{
	FOREACH(Decl *, d, vec)
	{
		if (d == target)
		{
			return true;
		}
	}
	return false;
}

/* Statements */
void c_emit_local_decl(GenContext *c, Decl *decl, CValue *value);
void c_emit_check_fault_and_return(GenContext *c, Expr *expr);
void c_emit_stmt(GenContext *c, Ast *stmt);
void c_emit_stmt_chain(GenContext *c, AstId current);
void c_emit_asm_block_stmt(GenContext *c, Ast *stmt);

/* Types */
Decl *c_decl_unwrap(Decl *decl);
bool is_valid_type_ptr(Type *type);
Type *c_unwrap_alias(Type *type);
Type *c_safe_type_lower(Type *type);
Type *c_decl_type(Decl *decl);
Type *c_expr_type(Expr *expr);
bool c_type_is_aggregate(Type *t);
bool c_type_is_resolved(Type *type);
int c_get_type_introspection_kind(Type *type);
size_t c_get_type_size(Type *type);
const char *c_type_name(GenContext *c, Type *type);
const char *c_type_zero_literal(Type *t);
bool c_emit_type_forward_decl(GenContext *c, Type *type);
bool c_emit_type_decl(GenContext *c, Type *type);
const char *c_typeid_name(Type *type);
const char *c_fault_symbol_name(Decl *decl);
Type *c_get_method_target_type(Decl *m);

static inline bool c_type_needs_emission(Type *t)
{
	return is_valid_type_ptr(t) && t != poisoned_type &&
	       t->type_kind != TYPE_POISONED && t->type_kind != TYPE_WILDCARD &&
	       t->type_kind != TYPE_UNTYPEDLIST && t->type_kind != TYPE_TYPEINFO &&
	       t->type_kind != TYPE_MEMBER && t->type_kind != TYPE_REFLECTION;
}

/* Expressions & Builtins */
void c_emit_expr(GenContext *c, CValue *value, Expr *expr);
void c_emit_ignored_expr(GenContext *c, Expr *expr);
void c_emit_builtin_call(GenContext *c, CValue *value, Expr *expr);

#endif /* C_CODEGEN_INTERNAL_H */