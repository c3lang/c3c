#ifndef C_CODEGEN_INTERNAL_H
#define C_CODEGEN_INTERNAL_H

#include "codegen_internal.h"
#include <inttypes.h>

#include "../utils/byte_buffer.h"

#define PRINT(s) c_print(c, (s))
#define PRINTF(...) c_printf(c, __VA_ARGS__)

typedef int VariableId;

// Variable and label ids stored in pointer fields. Ids start at 1, so NULL means none.
static inline void *c_id_to_ptr(VariableId id) { return (void *)(uintptr_t)id; } // NOLINT(performance-no-int-to-ptr)
static inline VariableId c_ptr_to_id(void *ptr) { return (VariableId)(uintptr_t)ptr; }

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
	// The fault slot of an optional variable whose value this is (EXPR_DECL), or NULL. Reading the value checks it and
	// jumps to the catch target first, as LLVM's BE_ADDRESS_OPTIONAL folds on rvalue.
	const char *fault;
	// The alignment of the address an lvalue address points at, when lower than its type's (a @packed member), or
	// 0. Loads and stores through it then go through memcpy, as LLVM loads with a lower alignment.
	AlignSize align;
} CValue;

static inline void c_value_set_void(CValue *val)
{
	if (val)
	{
		val->var  = 0;
		val->type = type_void;
		val->kind = CV_VALUE;
	}
}

typedef struct
{
	VariableId id;
	Type *type;
	bool is_static;
	bool is_alloca;
	ArrayIndex alloca_count;
	Decl *decl;
} CLocalVar;

// Where an optional expression goes when it evaluates to a fault, as in the LLVM backend.
// `label` is the jump target (0 when no fault can be handled here),
// `fault` the C lvalue that receives the fault, or NULL when the fault itself is not needed.
typedef struct
{
	int label;
	const char *fault;
} CCatch;

#define NO_CATCH ((CCatch){ 0, NULL })

// The C backend's view of a macro block, stored through the BlockExit pointer shared with block exit statements.
// `return_out` receives the value (0 if none), `return_exit` is the label after the block,
// `catch` the catch target that was active around the block.
typedef struct
{
	int return_out;
	Type *return_type;
	int return_exit;
	CCatch catch;
} CBlockExit;

// How a signature returns. value == NULL: nothing comes back (void / zero sized).
// An optional returns its fault, the value goes through a leading `T *___ret_out` parameter.
typedef struct
{
	bool optional;
	Type *value;
} CRet;

typedef struct GenContext
{
	FILE *file;
	ByteBuffer *buffer;
	const char *base_name;
	const char *c_filename;
	size_t c_file_size;
	const char *object_filename;
	Module *current_module;
	int id_gen;
	bool current_block_live;

	CRet cur_ret;
	// Make c_traverse_all_modules visit interfaces that are not live (see c_emit_dynamic_dispatcher).
	bool traverse_unused_interfaces;
	// The catch target of the expression being emitted.
	CCatch catch;
	// The fault being propagated while emitting `defer catch` / `defer (catch err)` statements.
	const char *defer_error_var;
	int current_break_label;
	int current_continue_label;
	CValue retval;

	CLocalVar *function_locals;
	const char *cur_func_name;
	Decl *cur_func_decl;

	// A file declares only what its code uses. Type declarations, other declarations and dispatchers are written to
	// their own buffers, which go in front of the code; types named while recording get full definitions, between
	// the type declarations and the rest (c_type_name).
	ByteBuffer *type_decl_buffer;
	ByteBuffer *decl_buffer;
	ByteBuffer *dispatch_buffer;
	bool record_types;
	HTable used_type_set;
	Type **used_types;

	HTable gen_decl;
	HTable gen_def;
	HTable local_vars;
	HTable local_fault_vars;
	HTable declared_vars;
	// The C names of variables with a C3 name, which c_write_mapped_lines gives ___var_N. A function uses each
	// name once: used_var_names maps a name to the last name_scope (a function) that took it.
	HTable var_names;
	HTable used_var_names;
	HTable used_static_names;
	int name_scope;
	HTable decl_names;
	HTable declared_typeids;
	HTable declared_faults;
	HTable declared_bitstructs;
	HTable emitted_func_defs;
	HTable emitted_global_decls;
	HTable emitted_global_defs;
} GenContext;

// Direct output to a declaration buffer, if the file has one; returns the buffer to restore.
static inline ByteBuffer *c_decl_stream_enter(GenContext *c, ByteBuffer *stream)
{
	ByteBuffer *prev = c->buffer;
	if (stream) c->buffer = stream;
	return prev;
}

VariableId c_emit_alloca_array(GenContext *c, Type *base_type, ArrayIndex count);

typedef void (*CDeclVisitor)(GenContext *c, Decl *d, void *userdata);

/* Output */
void c_write_bytes(GenContext *c, const uint8_t *bytes, size_t len);
void c_print(GenContext *c, const char *str);
// With debug info, maps the C that follows to `loc` (c_write_mapped_lines); C_LINE_RESET returns to the C file.
void c_emit_line(GenContext *c, SourceLocId loc);
#define C_LINE_RESET "#line __c3_reset__\n"
void c_printf(GenContext *c, const char *fmt, ...);

/* Module & Utility */
const char *c_intern(const char *str);
const char *c_sanitize_name(const char *name);
const char *c_get_decl_name(Decl *decl);
const char *c_get_decl_asm_name(Decl *decl);
bool c_decl_needs_asm_label(Decl *decl);
const char *c_decl_ext_symbol(Decl *decl);
bool c_fn_is_main(Decl *fn);
const char *c_decl_lvalue_str(GenContext *c, Decl *decl);
// The name of a function or global the code refers to, declared in the file on first use.
const char *c_decl_ref(GenContext *c, Decl *decl);
const char *c_decl_export_attrs(Decl *decl, bool is_definition);
const char *c_get_enum_assoc_name(Decl *enum_decl, Decl *param);
void c_emit_string_literal(GenContext *c, const char *bytes, ArrayIndex len);
void c_traverse_all_modules(GenContext *c, CDeclVisitor visitor, void *userdata);
void c_emit_panic(GenContext *c, const char *message, SourceLocId loc, Expr **args);
// A value passed to a compiler generated panic: a C expression and its type.
typedef struct
{
	const char *expr;
	Type *type;
} CPanicValue;
void c_emit_panic_fn(GenContext *c, PanicFunction id, SourceLocId loc, const char **texts, CPanicValue *values);
void c_emit_panic_if(GenContext *c, const char *cond, PanicFunction id, SourceLocId loc, const char *text, CPanicValue *value_1, CPanicValue *value_2);

/* Function & Variable Management */
// A function only referenced inside its own module gets internal linkage, as in the LLVM backend.
// The module is a single C file, so `static` is enough.
// Generated glue in other files calls @init functions and dynamic methods, so those stay external.
static inline bool c_fn_is_internal(Decl *fn)
{
	if (!decl_is_local(fn) || fn->is_extern || fn->is_weak || !fn->func_decl.body) return false;
	return !fn->func_decl.attr_init && !fn->func_decl.attr_finalizer && !fn->func_decl.attr_dynamic;
}
// A global only referenced inside its own module, as in the LLVM backend.
static inline bool c_global_is_internal(Decl *var)
{
	return decl_is_local(var) && !var->is_extern && !var->is_weak_link;
}
int c_create_label(GenContext *c);
int c_create_variable(GenContext *c);
void c_register_function_local(GenContext *c, VariableId id, Type *type, bool is_static, Decl *decl);
VariableId c_emit_temp_var(GenContext *c, CValue *value, Type *type);
VariableId c_get_or_create_decl_var(GenContext *c, Decl *decl);
VariableId c_get_decl_fault_var(GenContext *c, Decl *decl);
const char *c_decl_fault_ref(GenContext *c, Decl *decl);
typedef struct
{
	const char **names; // one per sig->params entry, NULL = types only
	Type *self_type;    // replaces the type of params[0] (dispatcher: any), NULL = none
	bool is_main;       // params 1 and 2 print as char**
} CSigOpts;
// `ret declarator(params)` for a signature. declarator: "name", "(*)", "(*name)".
const char *c_signature_str(GenContext *c, Signature *sig, const char *declarator, CSigOpts opts);
CRet c_signature_ret(Signature *sig);
const char *c_ident_read_fault_ref(GenContext *c, Decl *ident);
void c_emit_jump_to_optional_exit(GenContext *c, const char *fault);
void c_emit_fault_to_optional_exit(GenContext *c, const char *fault);
// State for storing an expression into an optional variable, see c_optional_assign_begin.
typedef struct
{
	CCatch saved;
	const char *fault_ref;
	int after;
	int rejump;
} COptionalAssign;
void c_optional_assign_begin(GenContext *c, COptionalAssign *assign, const char *fault_ref, Expr *rhs);
void c_optional_assign_end(GenContext *c, COptionalAssign *assign);
VariableId c_ensure_cvalue_var(GenContext *c, CValue *val, Type *fallback_type);
void c_value_rvalue(GenContext *c, CValue *val);
void c_value_addr(GenContext *c, CValue *val);
const char *c_addr_bytes(const CValue *addr);
const char *c_arrow(const CValue *val);
void c_emit_lanewise(GenContext *c, int result_var, Type *result_type, const char *tmpl, CValue *args, int arg_count);
void c_emit_label(GenContext *c, int label_id);
void c_emit_var_zero_init(GenContext *c, VariableId var_id, Type *type);
void c_emit_store(GenContext *c, const char *dst, Type *type, CValue *src);
void c_emit_assign_var(GenContext *c, VariableId dst_var, Type *dst_type, CValue *src_val);
bool c_emit_function_decl(GenContext *c, Decl *fn, bool is_current_module);
void c_emit_function(GenContext *c, Decl *fn);
void c_emit_local_var_declarations(GenContext *c);
void c_declare_local_here(GenContext *c, VariableId id);
const char *c_align_attr_str(Decl *decl, Type *type);

/* Lvalue & Access */
Decl *get_struct_decl_from_type(Type *type);
#define MAX_MEMBER_PATH 16
int c_member_path(Decl *parent, Decl *member, int indices[MAX_MEMBER_PATH]);
void c_declare_bitstruct_accessors(GenContext *c, Decl *decl);
const char *c_emit_bitstruct_container(GenContext *c, Expr *parent_expr, Decl *member, int *out_container_var, Decl **out_bitstruct_decl);
void c_emit_lvalue_addr(GenContext *c, Expr *expr, CValue *out_val, Type *target_type);
void c_emit_lvalue_read(GenContext *c, Expr *expr, CValue *out_val, Type *target_type);
void c_emit_lvalue_assign(GenContext *c, Expr *left, CValue *addr, CValue *right_val, CValue *out_val);

/* Global & Runtime */
bool c_is_file_global(Decl *decl);
bool c_can_emit_static_initializer(Expr *expr);
bool c_const_init_needs_stores(ConstInitializer *init);
void c_emit_const_stores(GenContext *c, const char *lvalue, ConstInitializer *init);
void c_emit_const_initializer_rec(GenContext *c, ConstInitializer *init);
void c_emit_const_init_expr(GenContext *c, Expr *expr, Type *type);
void c_emit_global_decl(GenContext *c, Decl *var);
void c_emit_enum_assoc_decl(GenContext *c, Decl *enum_decl, Decl *param);
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

static inline bool c_type_is_vec_or_arr(Type *t)
{
	if (!t)
	{
		return false;
	}
	t = type_flatten(t);
	return t && (t->type_kind == TYPE_ARRAY || t->type_kind == TYPE_VECTOR || t->type_kind == TYPE_SIMD_VECTOR);
}

static inline bool c_type_is_any(Type *t)
{
	if (!t)
	{
		return false;
	}
	t = type_flatten(t);
	return t && (t->type_kind == TYPE_ANY || t->type_kind == TYPE_INTERFACE);
}

static inline bool c_type_is_c_pointer(Type *t)
{
	if (!t)
	{
		return false;
	}
	t = type_flatten(t);
	return t && (type_is_pointer(t) || t->type_kind == TYPE_FUNC_PTR ||
	             t->type_kind == TYPE_ANYFAULT || t->type_kind == TYPE_TYPEID);
}

static inline const char *c_binary_op_str(BinaryOp op)
{
	switch (op)
	{
		case BINARYOP_MULT: return "*";
		case BINARYOP_DIV: return "/";
		case BINARYOP_MOD: return "%";
		case BINARYOP_ADD: return "+";
		case BINARYOP_SUB: return "-";
		case BINARYOP_SHL: return "<<";
		case BINARYOP_SHR: return ">>";
		case BINARYOP_BIT_AND: return "&";
		case BINARYOP_BIT_XOR: return "^";
		case BINARYOP_BIT_OR: return "|";
		case BINARYOP_EQ:
		case BINARYOP_VEC_EQ: return "==";
		case BINARYOP_NE:
		case BINARYOP_VEC_NE: return "!=";
		case BINARYOP_LT:
		case BINARYOP_VEC_LT: return "<";
		case BINARYOP_LE:
		case BINARYOP_VEC_LE: return "<=";
		case BINARYOP_GT:
		case BINARYOP_VEC_GT: return ">";
		case BINARYOP_GE:
		case BINARYOP_VEC_GE: return ">=";
		default: return "+";
	}
}

/* Statements */
void c_emit_local_decl(GenContext *c, Decl *decl, CValue *value);
void c_emit_fault_return(GenContext *c, const char *fault);
void c_emit_stmt(GenContext *c, Ast *stmt);
void c_emit_stmt_chain(GenContext *c, AstId current);
void c_emit_asm_block_stmt(GenContext *c, Ast *stmt);

/* Types */
Decl *c_decl_unwrap(Decl *decl);

Type *c_unwrap_alias(Type *type);
Type *c_safe_type_lower(Type *type);
Type *c_decl_type(Decl *decl);
Type *c_expr_type(Expr *expr);
bool c_type_is_aggregate(Type *t);
bool c_type_is_resolved(Type *type);
size_t c_get_type_size(Type *type);
void c_type_names_init(void);
const char *c_type_name(GenContext *c, Type *type);
const char *c_type_zero_literal(Type *t);
AlignSize c_type_alignment(Type *type);
bool c_struct_is_packed(Decl *d);
bool c_member_is_bytes(Decl *d, Decl *member);
bool c_name_is_reserved(const char *name);
const char *c_member_name(Decl *d, int index);
const char *c_member_path_str(Decl *parent, const int *indices, int depth);
bool c_type_is_int128(Type *type);
const char *c_int_value_str(GenContext *c, Type *type, int64_t value);
const char *c_scalar_convert_str(GenContext *c, Type *to, Type *from, const char *expr);
const char *c_binary_str(BinaryOp op, Type *type, const char *l, const char *r);
const char *c_truth_str(Type *type, const char *x, bool negate);
const char *c_int_neg_str(Type *type, const char *x);
const char *c_int_limit_str(GenContext *c, Type *type, bool is_max);
const char *c_shift_amount_str(Type *type, const char *expr);
const char *c_overflow_builtin_name(BinaryOp op, Type *type);
bool c_emit_type_forward_decl(GenContext *c, Type *type);
bool c_emit_type_decl(GenContext *c, Type *type);
const char *c_typeid_name(Type *type);
const char *c_typeid_ref(GenContext *c, Type *type);
const char *c_fault_ref(GenContext *c, Decl *decl);
const char *c_fault_symbol_name(Decl *decl);
Type *c_get_method_target_type(Decl *m);

static inline Decl *c_type_get_decl(Type *t)
{
	switch (t->type_kind)
	{
		case TYPE_STRUCT:
		case TYPE_UNION:
		case TYPE_BITSTRUCT:
		case TYPE_ENUM:
		case TYPE_CONSTSET:
		case TYPE_TYPEDEF:
		case TYPE_INTERFACE:
			return t->decl;
		default:
			return NULL;
	}
}

// Whether a typeid can exist for the type at all.
static inline bool c_type_has_typeid(Type *t)
{
	if (t->type_kind == TYPE_POISONED || t->type_kind == TYPE_WILDCARD ||
	    t->type_kind == TYPE_UNTYPEDLIST || t->type_kind == TYPE_TYPEINFO ||
	    t->type_kind == TYPE_MEMBER || t->type_kind == TYPE_REFLECTION ||
	    t->type_kind == TYPE_FUNC_RAW || t->type_kind == TYPE_INFERRED_ARRAY ||
	    t->type_kind == TYPE_INFERRED_VECTOR)
	{
		return false;
	}
	Decl *d = c_type_get_decl(t);
	ASSERT(!d || (d->unit && d->unit->module));
	return true;
}

/* Expressions & Builtins */
void c_emit_expr(GenContext *c, CValue *value, Expr *expr);
bool c_param_by_ref(Type *type);
void c_emit_binary_op(GenContext *c, CValue *value, BinaryOp op, Type *expr_type, CValue left_value, CValue right_value, SourceLocId loc);
void c_emit_ignored_expr(GenContext *c, Expr *expr);
void c_emit_builtin_call(GenContext *c, CValue *value, Expr *expr);
void c_emit_inc_dec(GenContext *c, CValue *value, Expr *expr, bool is_post);

/* Typeid Tracking */
extern HTable c_needed_typeids;
extern Type **c_needed_typeids_vec;
extern bool c_recording_typeids;
void c_record_needed_typeid(Type *type, const char *sym);

#endif /* C_CODEGEN_INTERNAL_H */