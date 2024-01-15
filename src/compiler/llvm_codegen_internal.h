#pragma once

// Copyright (c) 2019 Christoffer Lerno. All rights reserved.
// Use of this source code is governed by the GNU LGPLv3.0 license
// a copy of which can be found in the LICENSE file.

#include "codegen_internal.h"
#include <llvm-c/Core.h>
#include <llvm-c/Analysis.h>
#include <llvm-c/ExecutionEngine.h>
#include <llvm-c/Target.h>
#include <llvm-c/Analysis.h>
#include <llvm-c/BitWriter.h>
#include <llvm-c/DebugInfo.h>
#include "dwarf.h"

#define SLICE_MAX_UNROLL 4

extern const char *varargslots_name;
extern const char *temp_name;
typedef enum
{
	BE_VALUE,
	BE_ADDRESS,
	BE_ADDRESS_OPTIONAL,
	BE_BOOLEAN,
	BE_BOOLVECTOR,
} BackendValueKind;

typedef struct
{
	BackendValueKind kind : 5;
	AlignSize alignment;
	Type *type; // Should never be a distinct or canonical type.
	LLVMValueRef value;
	LLVMValueRef optional;
} BEValue;

typedef struct
{
	LLVMBasicBlockRef continue_block;
	LLVMBasicBlockRef break_block;
	LLVMBasicBlockRef next_block;
}  BreakContinue;

typedef struct DebugFile_
{
	FileId file_id;
	LLVMMetadataRef debug_file;
} DebugFile;

typedef struct
{
	unsigned runtime_version : 8;
	bool enable_stacktrace : 1;
	LLVMDIBuilderRef builder;
	DebugFile *debug_files;
	DebugFile file;
	LLVMMetadataRef compile_unit;
	LLVMMetadataRef function;
	LLVMMetadataRef *lexical_block_stack;
	LLVMMetadataRef inlined_at;
} DebugContext;



typedef struct ReusableConstant_
{
	const char *string;
	const char *name;
	LLVMValueRef value;
} ReusableConstant;
typedef struct GenContext_
{
	bool shared_context;
	LLVMModuleRef module;
	LLVMBuilderRef global_builder;
	LLVMTargetMachineRef machine;
	LLVMTargetDataRef target_data;
	LLVMContextRef context;
	LLVMValueRef function;
	LLVMValueRef alloca_point;
	LLVMBuilderRef builder;
	LLVMBasicBlockRef current_block;
	LLVMBasicBlockRef catch_block;
	LLVMBasicBlockRef *panic_blocks;
	LLVMValueRef *constructors;
	LLVMValueRef *destructors;
	ReusableConstant *reusable_constants;
	const char *ir_filename;
	const char *object_filename;
	const char *asm_filename;
	// The recipient of the error value in a catch err = ... expression.
	LLVMValueRef opt_var;
	LLVMTypeRef bool_type;
	LLVMTypeRef byte_type;
	LLVMTypeRef introspect_type;
	LLVMTypeRef fault_type;
	LLVMTypeRef size_type;
	LLVMTypeRef typeid_type;
	LLVMTypeRef dtable_type;
	LLVMTypeRef ptr_type;
	LLVMTypeRef chars_type;
	LLVMTypeRef xtor_entry_type;
	LLVMTypeRef xtor_func_type;
	Decl *panic_var;
	Decl *panicf;
	struct
	{
		const char *name;
		FunctionPrototype *prototype;
		Type *rtype;
	} cur_func;
	int block_global_unique_count;
	int ast_alloca_addr_space;
	DebugContext debug;
	Module *code_module;
	LLVMValueRef return_out;
	LLVMValueRef optional_out;
	BEValue retval;
	bool current_block_is_target : 1;
	SourceSpan last_emitted_loc;
	Decl **dynamic_functions;
	LLVMValueRef dyn_find_function;
	LLVMTypeRef dyn_find_function_type;
	LLVMTypeRef dyn_section_type;
} GenContext;

// LLVM Intrinsics

typedef struct
{
	unsigned abs;
	unsigned assume;
	unsigned bitreverse;
	unsigned bswap;
	unsigned ceil;
	unsigned convert_from_fp16;
	unsigned convert_to_fp16;
	unsigned copysign;
	unsigned cos;
	unsigned ctlz;
	unsigned ctpop;
	unsigned cttz;
	unsigned exp;
	unsigned exp2;
	unsigned expect;
	unsigned expect_with_probability;
	unsigned fabs;
	unsigned floor;
	unsigned fma;
	unsigned fmuladd;
	unsigned frameaddress;
	unsigned fshl;
	unsigned fshr;
	unsigned gather;
	unsigned get_rounding;
	unsigned lifetime_end;
	unsigned lifetime_start;
	unsigned llrint;
	unsigned llround;
	unsigned log;
	unsigned log2;
	unsigned log10;
	unsigned lrint;
	unsigned lround;
	unsigned masked_compressstore;
	unsigned masked_expandload;
	unsigned masked_load;
	unsigned masked_store;
	unsigned maximum;
	unsigned maxnum;
	unsigned memcpy;
	unsigned memcpy_inline;
	unsigned memmove;
	unsigned memset;
	unsigned memset_inline;
	unsigned minimum;
	unsigned minnum;
	unsigned nearbyint;
	unsigned pow;
	unsigned powi;
	unsigned prefetch;
	unsigned readcyclecounter;
	unsigned returnaddress;
	unsigned rint;
	unsigned round;
	unsigned roundeven;
	unsigned sadd_overflow;
	unsigned sadd_sat;
	unsigned scatter;
	unsigned set_rounding;
	unsigned sin;
	unsigned smax;
	unsigned smin;
	unsigned smul_overflow;
	unsigned sqrt;
	unsigned sshl_sat;
	unsigned ssub_overflow;
	unsigned ssub_sat;
	unsigned trap;
	unsigned trunc;
	unsigned uadd_overflow;
	unsigned uadd_sat;
	unsigned umax;
	unsigned umin;
	unsigned umul_overflow;
	unsigned ushl_sat;
	unsigned usub_overflow;
	unsigned usub_sat;
	unsigned vector_reduce_fmax;
	unsigned vector_reduce_fmin;
	unsigned vector_reduce_smax;
	unsigned vector_reduce_smin;
	unsigned vector_reduce_umax;
	unsigned vector_reduce_umin;
	unsigned vector_reduce_add;
	unsigned vector_reduce_fadd;
	unsigned vector_reduce_mul;
	unsigned vector_reduce_fmul;
	unsigned vector_reduce_and;
	unsigned vector_reduce_or;
	unsigned vector_reduce_xor;
	unsigned vector_predicate_select;
	unsigned wasm_memory_size;
	unsigned wasm_memory_grow;
} LLVMIntrinsics;

extern LLVMIntrinsics intrinsic_id;

typedef struct
{
	unsigned afn;             // allow approximate functions
	unsigned align;           // align
	unsigned alwaysinline;    // Force inlining
	unsigned arcp;            // allow reciprocal
	unsigned byval;           // ByVal (param)
	unsigned contract;        // allow fused multiply-and-add
	unsigned elementtype;     // elementtype (type)
	unsigned fast;            // fast fp
	unsigned inlinehint;      // "Inline possibly"
	unsigned inreg;           // inreg (param)
	unsigned naked;           // naked function
	unsigned ninf;            // no infs
	unsigned nnan;            // no nans
	unsigned noalias;         // noalias (pointer)
	unsigned noinline;        // No function inlining
	unsigned noreturn;        // No function return
	unsigned nounwind;        // No exceptions
	unsigned nsz;             // no signed zeros
	unsigned optnone;         // No optimization
	unsigned readonly;        // No reads on pointer
	unsigned reassoc;         // allow reassociateion
	unsigned sext;            // sign extend
	unsigned sret;            // struct return pointer
	unsigned ssp;             // safe stack protection
	unsigned target_features; // target-features for function compilation
	unsigned uwtable;
	unsigned writeonly;       // No writes on pointer
	unsigned zext;            // zero extend
} LLVMAttributes;

extern LLVMAttributes attribute_id;
// LLVM Attributes

void gencontext_begin_module(GenContext *c);
void gencontext_init_file_emit(GenContext *c, CompilationUnit *unit);
void gencontext_end_file_emit(GenContext *c, CompilationUnit *ast);
void gencontext_end_module(GenContext *context);

// Patched functions
LLVMValueRef LLVMConstBswap(LLVMValueRef ConstantVal);
void LLVMBuilderSetFastMathFlags(LLVMBuilderRef Builder, FpOpt option);

#ifndef LLVMCreateTypeAttribute
LLVMAttributeRef LLVMCreateTypeAttribute(LLVMContextRef C, unsigned KindID,
										 LLVMTypeRef type_ref);
#endif

LLVMBuilderRef llvm_create_builder(GenContext *c);

static inline LLVMValueRef decl_optional_ref(Decl *decl)
{
	assert(decl->decl_kind == DECL_VAR);
	if (decl->var.kind == VARDECL_UNWRAPPED) return decl_optional_ref(decl->var.alias);
	if (decl->type->type_kind != TYPE_OPTIONAL) return NULL;
	return decl->var.optional_ref;
}

INLINE bool llvm_is_global_eval(GenContext *c);
INLINE bool llvm_is_local_eval(GenContext *c);


// -- BE value --
void llvm_value_addr(GenContext *c, BEValue *value);
static inline bool llvm_value_is_addr(BEValue *value) { return value->kind == BE_ADDRESS || value->kind == BE_ADDRESS_OPTIONAL; }
static inline bool llvm_value_is_bool(BEValue *value) { return value->kind == BE_BOOLEAN; }
bool llvm_value_is_const(BEValue *value);
void llvm_value_rvalue(GenContext *context, BEValue *value);
void llvm_value_deref(GenContext *c, BEValue *value);
void llvm_value_set(BEValue *value, LLVMValueRef llvm_value, Type *type);
void llvm_value_set_int(GenContext *c, BEValue *value, Type *type, uint64_t i);
void llvm_value_set_address(BEValue *value, LLVMValueRef llvm_value, Type *type, AlignSize alignment);
void llvm_value_set_address_abi_aligned(BEValue *value, LLVMValueRef llvm_value, Type *type);
void llvm_value_set_decl_address(GenContext *c, BEValue *value, Decl *decl);
void llvm_value_set_decl(GenContext *c, BEValue *value, Decl *decl);
void llvm_value_fold_optional(GenContext *c, BEValue *value);
void llvm_value_struct_gep(GenContext *c, BEValue *element, BEValue *struct_pointer, unsigned index);
INLINE void llvm_value_bitcast(GenContext *c, BEValue *value, Type *type);
void llvm_value_aggregate_two(GenContext *c, BEValue *value, Type *type, LLVMValueRef value1, LLVMValueRef value2);

// -- Types --
LLVMValueRef llvm_get_typeid(GenContext *context, Type *type);
LLVMTypeRef llvm_abi_type(GenContext *c, AbiType type);
TypeSize llvm_abi_size(GenContext *c, LLVMTypeRef type);
BitSize llvm_bitsize(GenContext *c, LLVMTypeRef type);
AlignSize llvm_abi_alignment(GenContext *c, LLVMTypeRef type);
LLVMTypeRef llvm_func_type(GenContext *context, FunctionPrototype *prototype);
LLVMTypeRef llvm_update_prototype_abi(GenContext *c, FunctionPrototype *prototype, LLVMTypeRef **params);
LLVMTypeRef llvm_get_twostruct(GenContext *context, LLVMTypeRef lo, LLVMTypeRef hi);
LLVMTypeRef llvm_const_padding_type(GenContext *c, TypeSize size);
LLVMMetadataRef llvm_get_debug_type(GenContext *c, Type *type);
LLVMTypeRef llvm_get_type(GenContext *c, Type *any_type);
LLVMTypeRef llvm_get_pointee_type(GenContext *c, Type *any_type);
void llvm_emit_function_decl(GenContext *c, Decl *decl);
bool llvm_types_are_similar(LLVMTypeRef original, LLVMTypeRef coerce);

// -- Attributes ---
void llvm_attribute_add_range(GenContext *c, LLVMValueRef value_to_add_attribute_to, unsigned attribute, int index_start, int index_end);
void llvm_attribute_add(GenContext *c, LLVMValueRef value_to_add_attribute_to, unsigned attribute, int index);
void llvm_attribute_add_call(GenContext *context, LLVMValueRef call, unsigned attribute, int index, int64_t value);
void llvm_attribute_add_call_type(GenContext *c, LLVMValueRef call, unsigned attribute, int index, LLVMTypeRef type);
void llvm_attribute_add_string(GenContext *c, LLVMValueRef value_to_add_attribute_to, const char *attribute, const char *value, int index);
void llvm_attribute_add_type(GenContext *c, LLVMValueRef value_to_add_attribute_to, unsigned attribute, LLVMTypeRef type, int index);
void llvm_attribute_add_int(GenContext *c, LLVMValueRef value_to_add_attribute_to, unsigned attribute, uint64_t val, int index);

// -- Linking --
void llvm_set_linkonce(GenContext *c, LLVMValueRef global);
void llvm_set_comdat(GenContext *c, LLVMValueRef global);
void llvm_set_weak(GenContext *c, LLVMValueRef global);
void llvm_set_private_linkage(LLVMValueRef alloc);
void llvm_set_internal_linkage(LLVMValueRef alloc);
void llvm_set_global_tls(Decl *decl);

// -- Globals --
INLINE LLVMValueRef llvm_add_global_raw(GenContext *c, const char *name, LLVMTypeRef type, AlignSize alignment);
INLINE LLVMValueRef llvm_add_global(GenContext *c, const char *name, Type *type, AlignSize alignment);
void llvm_add_global_decl(GenContext *c, Decl *decl);
void llvm_emit_global_variable_init(GenContext *c, Decl *decl);

// -- Alloca --
LLVMValueRef llvm_emit_alloca(GenContext *c, LLVMTypeRef type, unsigned alignment, const char *name);
LLVMValueRef llvm_emit_alloca_aligned(GenContext *c, Type *type, const char *name);
void llvm_emit_and_set_decl_alloca(GenContext *c, Decl *decl);
INLINE void llvm_set_alignment(LLVMValueRef alloca, AlignSize alignment);
INLINE AlignSize llvm_type_or_alloca_align(LLVMValueRef dest, Type *type);

INLINE LLVMValueRef llvm_zext_trunc(GenContext *c, LLVMValueRef data, LLVMTypeRef type);
INLINE UNUSED LLVMValueRef llvm_sext_trunc(GenContext *c, LLVMValueRef data, LLVMTypeRef type);
INLINE void llvm_value_ext_trunc(GenContext *c, BEValue *value, Type *type);

// -- Constants --
void llvm_emit_typeid(GenContext *c, BEValue *be_value, Type *type);
LLVMValueRef llvm_emit_const_initializer(GenContext *c, ConstInitializer *const_init);
LLVMValueRef llvm_emit_const_padding(GenContext *c, AlignSize size);
LLVMValueRef llvm_emit_string_const(GenContext *c, const char *str, const char *extname);
LLVMValueRef llvm_emit_empty_string_const(GenContext *c);
UNUSED LLVMValueRef llvm_emit_zstring(GenContext *c, const char *str);
LLVMValueRef llvm_emit_zstring_named(GenContext *c, const char *str, const char *extname);
INLINE LLVMValueRef llvm_const_int(GenContext *c, Type *type, uint64_t val);
INLINE LLVMValueRef llvm_get_zero(GenContext *c, Type *type);
INLINE LLVMValueRef llvm_get_zero_raw(LLVMTypeRef type);
INLINE LLVMValueRef llvm_get_undef(GenContext *c, Type *type);
INLINE LLVMValueRef llvm_get_undef_raw(LLVMTypeRef type);
INLINE LLVMValueRef llvm_get_ones_raw(LLVMTypeRef type);
INLINE LLVMValueRef llvm_get_zstring(GenContext *c, const char *str, size_t len);
INLINE LLVMValueRef llvm_get_bytes(GenContext *c, const char *str, size_t len);
INLINE LLVMValueRef llvm_get_struct(GenContext *c, LLVMValueRef *vals, size_t len);
INLINE LLVMValueRef llvm_get_packed_struct(GenContext *c, LLVMValueRef *vals, size_t len);
INLINE LLVMValueRef llvm_get_unnamed_struct(GenContext *c, LLVMValueRef *vals, bool is_packed);
INLINE LLVMValueRef llvm_get_array(LLVMTypeRef type, LLVMValueRef *vals, unsigned count);
INLINE LLVMValueRef llvm_get_struct_named(LLVMTypeRef type, LLVMValueRef *vals, unsigned count);
INLINE LLVMValueRef llvm_get_struct_of_type(GenContext *c, Type *type, LLVMValueRef *vals, unsigned count);

// -- Jumps --
void llvm_emit_cond_br(GenContext *context, BEValue *value, LLVMBasicBlockRef then_block, LLVMBasicBlockRef else_block);
void llvm_emit_cond_br_raw(GenContext *context, LLVMValueRef b, LLVMBasicBlockRef then_block, LLVMBasicBlockRef else_block);
void llvm_emit_br(GenContext *c, LLVMBasicBlockRef next_block);
void llvm_emit_jump_to_optional_exit(GenContext *c, LLVMValueRef opt_value);
void llvm_emit_return_abi(GenContext *c, BEValue *return_value, BEValue *optional);
void llvm_emit_return_implicit(GenContext *c);

// -- Blocks --
LLVMBasicBlockRef llvm_basic_block_new(GenContext *c, const char *name);
void llvm_emit_block(GenContext *c, LLVMBasicBlockRef next_block);
static inline LLVMBasicBlockRef llvm_get_current_block_if_in_use(GenContext *context);
INLINE bool llvm_basic_block_is_unused(LLVMBasicBlockRef block);
bool llvm_emit_check_block_branch(GenContext *context);

// -- Comparisons ---
void llvm_emit_lhs_is_subtype(GenContext *c, BEValue *result, BEValue *lhs, BEValue *rhs);
void llvm_emit_comp(GenContext *c, BEValue *result, BEValue *lhs, BEValue *rhs, BinaryOp binary_op);
void llvm_emit_int_comp(GenContext *c, BEValue *result, BEValue *lhs, BEValue *rhs, BinaryOp binary_op);
void llvm_emit_int_comp_zero(GenContext *c, BEValue *result, BEValue *lhs, BinaryOp binary_op);
void llvm_emit_int_comp_raw(GenContext *c, BEValue *result, Type *lhs_type, Type *rhs_type, LLVMValueRef lhs_value, LLVMValueRef rhs_value, BinaryOp binary_op);
INLINE bool llvm_is_const_null(LLVMValueRef value);
INLINE bool llvm_is_const(LLVMValueRef value);

// -- Load ---
LLVMValueRef llvm_load(GenContext *c, LLVMTypeRef type, LLVMValueRef pointer, AlignSize alignment, const char *name);
LLVMValueRef llvm_load_abi_alignment(GenContext *c, Type *type, LLVMValueRef pointer, const char *name);
LLVMValueRef llvm_load_value(GenContext *c, BEValue *value);
LLVMValueRef llvm_load_value_store(GenContext *c, BEValue *value);

// -- Store ---
LLVMValueRef llvm_store(GenContext *c, BEValue *destination, BEValue *value);
LLVMValueRef llvm_store_zero(GenContext *c, BEValue *ref);
INLINE LLVMValueRef llvm_store_raw(GenContext *c, BEValue *destination, LLVMValueRef raw_value);
INLINE LLVMValueRef llvm_store_decl(GenContext *c, Decl *decl, BEValue *value);
INLINE LLVMValueRef llvm_store_decl_raw(GenContext *context, Decl *decl, LLVMValueRef value);
INLINE LLVMValueRef llvm_store_to_ptr(GenContext *c, LLVMValueRef destination, BEValue *value);
INLINE LLVMValueRef llvm_store_to_ptr_raw(GenContext *context, LLVMValueRef pointer, LLVMValueRef value, Type *type);
LLVMValueRef llvm_store_to_ptr_aligned(GenContext *c, LLVMValueRef destination, BEValue *value, AlignSize alignment);
LLVMValueRef llvm_store_to_ptr_raw_aligned(GenContext *context, LLVMValueRef pointer, LLVMValueRef value, AlignSize alignment);
void llvm_store_to_ptr_zero(GenContext *context, LLVMValueRef pointer, Type *type);
TypeSize llvm_store_size(GenContext *c, LLVMTypeRef type);
TypeSize llvm_alloc_size(GenContext *c, LLVMTypeRef type);

/// -- Aggregates --
INLINE LLVMValueRef llvm_emit_insert_value(GenContext *c, LLVMValueRef agg, LLVMValueRef new_value, ArraySize index);
LLVMValueRef llvm_emit_aggregate_two(GenContext *c, Type *type, LLVMValueRef value1, LLVMValueRef value2);
LLVMValueRef llvm_emit_const_vector(LLVMValueRef value, ArraySize len);
LLVMValueRef llvm_emit_struct_gep_raw(GenContext *c, LLVMValueRef ptr, LLVMTypeRef struct_type, unsigned index,
                                      unsigned struct_alignment, AlignSize *alignment);
LLVMValueRef llvm_emit_array_gep_raw(GenContext *c, LLVMValueRef ptr, LLVMTypeRef array_type, unsigned index, AlignSize array_alignment, AlignSize *alignment);
LLVMValueRef llvm_emit_array_gep_raw_index(GenContext *c, LLVMValueRef ptr, LLVMTypeRef array_type, BEValue *index, AlignSize array_alignment, AlignSize *alignment);
LLVMValueRef llvm_emit_pointer_gep_raw(GenContext *c, LLVMTypeRef pointee_type, LLVMValueRef ptr, LLVMValueRef offset);
LLVMValueRef llvm_emit_ptradd_raw(GenContext *c, LLVMValueRef ptr, LLVMValueRef offset, ByteSize mult);
LLVMValueRef llvm_emit_ptradd_inbounds_raw(GenContext *c, LLVMValueRef ptr, LLVMValueRef offset, ByteSize mult);
LLVMValueRef llvm_emit_const_ptradd_inbounds_raw(GenContext *c, LLVMValueRef ptr, ByteSize offset);
LLVMValueRef llvm_emit_pointer_inbounds_gep_raw(GenContext *c, LLVMTypeRef pointee_type, LLVMValueRef ptr, LLVMValueRef offset);
LLVMTypeRef llvm_coerce_expand_hi_offset(GenContext *c, LLVMValueRef *addr, ABIArgInfo *info, AlignSize *align);
void llvm_emit_ptr_from_array(GenContext *c, BEValue *value);
void llvm_emit_struct_member_ref(GenContext *c, BEValue *struct_ref, BEValue *member_ref, unsigned member_id);
INLINE LLVMValueRef llvm_emit_extract_value(GenContext *c, LLVMValueRef agg, unsigned index);


// -- Int operations ---
LLVMValueRef llvm_emit_shl_fixed(GenContext *c, LLVMValueRef data, int shift);
LLVMValueRef llvm_emit_lshr_fixed(GenContext *c, LLVMValueRef data, int shift);
LLVMValueRef llvm_emit_ashr_fixed(GenContext *c, LLVMValueRef data, int shift);
INLINE LLVMValueRef llvm_emit_ashr(GenContext *c, LLVMValueRef value, LLVMValueRef shift);
INLINE LLVMValueRef llvm_emit_shl(GenContext *c, LLVMValueRef value, LLVMValueRef shift);
INLINE LLVMValueRef llvm_emit_lshr(GenContext *c, LLVMValueRef value, LLVMValueRef shift);
INLINE UNUSED LLVMValueRef llvm_emit_trunc(GenContext *c, LLVMValueRef value, Type *type);
INLINE LLVMValueRef llvm_emit_trunc_bool(GenContext *c, LLVMValueRef value);
INLINE LLVMValueRef llvm_emit_and(GenContext *c, BEValue *lhs, BEValue *rhs);
INLINE LLVMValueRef llvm_emit_and_raw(GenContext *c, LLVMValueRef lhs, LLVMValueRef rhs);
INLINE LLVMValueRef llvm_emit_or_raw(GenContext *c, LLVMValueRef lhs, LLVMValueRef rhs);

// -- Mem ops --
LLVMValueRef llvm_emit_memclear_size_align(GenContext *c, LLVMValueRef ptr, uint64_t size, AlignSize align);
LLVMValueRef llvm_emit_memcpy(GenContext *c, LLVMValueRef dest, unsigned dest_align, LLVMValueRef source, unsigned src_align, uint64_t len);
void llvm_emit_memcpy_to_decl(GenContext *c, Decl *decl, LLVMValueRef source, unsigned source_alignment);

// -- ABI --
LLVMTypeRef llvm_get_coerce_type(GenContext *c, ABIArgInfo *arg_info);
LLVMValueRef llvm_get_next_param(GenContext *context, unsigned *index);
void llvm_emit_convert_value_from_coerced(GenContext *c, BEValue *result, LLVMTypeRef coerced, LLVMValueRef value, Type *original_type);
void llvm_emit_coerce_store(GenContext *c, LLVMValueRef addr, AlignSize alignment, LLVMTypeRef coerced, LLVMValueRef value, LLVMTypeRef target_type);
LLVMValueRef llvm_emit_coerce(GenContext *c, LLVMTypeRef coerced, BEValue *value, Type *original_type);

static inline LLVMCallConv llvm_call_convention_from_call(CallABI abi);
void llvm_emit_raw_call(GenContext *c, BEValue *result_value, FunctionPrototype *prototype, LLVMTypeRef func_type, LLVMValueRef func, LLVMValueRef *args, unsigned arg_count, int inline_flag, LLVMValueRef error_var, bool sret_return, BEValue *synthetic_return_param);
void llvm_emit_parameter(GenContext *c, LLVMValueRef *args, unsigned *arg_count_ref, ABIArgInfo *info, BEValue *be_value, Type *type);

// -- Dynamic interface --
LLVMValueRef llvm_get_selector(GenContext *c, const char *name);

// -- C3 Lowering --
void llvm_emit_expr(GenContext *c, BEValue *value, Expr *expr);
LLVMValueRef llvm_emit_expr_to_rvalue(GenContext *c, Expr *expr);
LLVMValueRef llvm_emit_exprid_to_rvalue(GenContext *c, ExprId expr_id);
void llvm_emit_ignored_expr(GenContext *c, Expr *expr);
void llvm_emit_stmt(GenContext *c, Ast *ast);
void llvm_emit_panic_on_true(GenContext *c, LLVMValueRef value, const char *panic_name, SourceSpan loc,
							 const char *fmt, BEValue *value_1, BEValue *value_2);
void llvm_emit_panic_if_true(GenContext *c, BEValue *value, const char *panic_name, SourceSpan loc, const char *fmt, BEValue *value_1,
							 BEValue *value_2);
void llvm_emit_panic(GenContext *c, const char *message, SourceSpan loc, const char *fmt, BEValue *args);
void llvm_emit_unreachable(GenContext *c);
void llvm_emit_assume_raw(GenContext *c, LLVMValueRef assume_true);
LLVMValueRef llvm_emit_expect_raw(GenContext *c, LLVMValueRef expect_true);
LLVMValueRef llvm_emit_expect_false_raw(GenContext *c, LLVMValueRef expect_false);
void llvm_emit_any_from_value(GenContext *c, BEValue *value, Type *type);
void llvm_emit_subarray_len(GenContext *context, BEValue *subarray, BEValue *len);
void llvm_emit_subarray_pointer(GenContext *context, BEValue *subarray, BEValue *pointer);
void llvm_emit_compound_stmt(GenContext *c, Ast *ast);
LLVMValueRef llvm_emit_const_bitstruct(GenContext *c, ConstInitializer *initializer);
void llvm_emit_function_body(GenContext *context, Decl *decl);
void llvm_emit_dynamic_functions(GenContext *context, Decl **funcs);
BEValue llvm_emit_assign_expr(GenContext *c, BEValue *ref, Expr *expr, LLVMValueRef optional);
INLINE void llvm_emit_exprid(GenContext *c, BEValue *value, ExprId expr);
INLINE void llvm_emit_statement_chain(GenContext *c, AstId current);
void llvm_emit_initialize_reference_temporary_const(GenContext *c, BEValue *ref, ConstInitializer *initializer);
void llvm_emit_len_for_expr(GenContext *c, BEValue *be_value, BEValue *expr_to_len);
LLVMValueRef llvm_get_ref(GenContext *c, Decl *decl);
LLVMValueRef llvm_emit_call_intrinsic(GenContext *c, unsigned intrinsic, LLVMTypeRef *types, unsigned type_count, LLVMValueRef *values, unsigned arg_count);
void llvm_emit_cast(GenContext *c, CastKind cast_kind, Expr *expr, BEValue *value, Type *to_type, Type *from_type);
void llvm_emit_local_var_alloca(GenContext *c, Decl *decl);
void llvm_emit_local_decl(GenContext *c, Decl *decl, BEValue *value);
void llvm_emit_builtin_call(GenContext *c, BEValue *result_value, Expr *expr);

// -- Optional --
LLVMValueRef llvm_emit_is_no_opt(GenContext *c, LLVMValueRef error_value);
LLVMValueRef llvm_get_opt_ref(GenContext *c, Decl *decl);

#define PUSH_OPT() LLVMBasicBlockRef _old_catch = c->catch_block; LLVMValueRef _old_opt_var = c->opt_var
#define POP_OPT() c->catch_block = _old_catch; c->opt_var = _old_opt_var

// -- Debug --
LLVMMetadataRef llvm_get_debug_file(GenContext *c, FileId file_id);
#define DEBUG_PUSH_LEXICAL_SCOPE(c__, span__) do { if (llvm_use_debug(c__)) llvm_debug_push_lexical_scope(c__, span__); } while (0)
#define DEBUG_POP_LEXICAL_SCOPE(c__) do { if (llvm_use_debug(c__)) llvm_debug_scope_pop(c__); } while (0)
INLINE bool llvm_use_debug(GenContext *context);
void llvm_debug_scope_push(GenContext *context, LLVMMetadataRef debug_scope);
void llvm_debug_scope_pop(GenContext *context);
void llvm_debug_push_lexical_scope(GenContext *context, SourceSpan location);
LLVMMetadataRef llvm_debug_current_scope(GenContext *context);
void llvm_emit_debug_function(GenContext *c, Decl *decl);
void llvm_emit_debug_location(GenContext *c, SourceSpan location);
void llvm_emit_debug_parameter(GenContext *c, Decl *parameter, unsigned index);
void llvm_emit_debug_local_var(GenContext *c, Decl *var);
void llvm_emit_debug_global_var(GenContext *c, Decl *global);

#define FRAMEPOINTER (platform_target.arch == ARCH_TYPE_AARCH64 ? 1 : 2)
#define UWTABLE (active_target.arch_os_target == MACOS_AARCH64 ? 1 : 2)
#define EMIT_LOC(c, x) do { if (c->debug.builder) llvm_emit_debug_location(c, x->span); } while (0)
#define EMIT_SPAN(c, x) do { if (c->debug.builder) llvm_emit_debug_location(c, x); } while (0)

LLVMAtomicOrdering llvm_atomic_ordering(Atomicity atomicity);

// Implementations

#include "llvm_codegen_internal_impl.h"




