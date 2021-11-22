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
#include <llvm-c/Transforms/PassManagerBuilder.h>
#include <llvm-c/Transforms/InstCombine.h>
#include <llvm-c/Transforms/Vectorize.h>
#include <llvm-c/Transforms/Scalar.h>
#include <llvm-c/Transforms/IPO.h>
#include <llvm-c/Transforms/Utils.h>
#include <llvm-c/Comdat.h>
#include "dwarf.h"


#define SLICE_MAX_UNROLL 4

typedef enum
{
	BE_VALUE,
	BE_ADDRESS,
	BE_ADDRESS_FAILABLE,
	BE_BOOLEAN,
} BackendValueKind;

typedef struct
{
	BackendValueKind kind : 5;
	AlignSize alignment;
	Type *type; // Should never be a distinct or canonical type.
	LLVMValueRef value;
	LLVMValueRef failable;
} BEValue;

typedef struct
{
	LLVMBasicBlockRef continue_block;
	LLVMBasicBlockRef break_block;
	LLVMBasicBlockRef next_block;
}  BreakContinue;

typedef struct
{
	unsigned runtime_version : 8;
	LLVMDIBuilderRef builder;
	LLVMMetadataRef file;
	LLVMMetadataRef compile_unit;
	LLVMMetadataRef function;
	SourceSpan current_range;
	LLVMMetadataRef *lexical_block_stack;
	LLVMMetadataRef inlined_at;
} DebugContext;


typedef struct
{
	LLVMModuleRef module;
	LLVMTargetMachineRef machine;
	LLVMTargetDataRef target_data;
	LLVMContextRef context;
	LLVMValueRef function;
	LLVMValueRef alloca_point;
	LLVMBuilderRef builder;
	LLVMBasicBlockRef current_block;
	LLVMBasicBlockRef catch_block;
	char *ir_filename;
	char *object_filename;
	// The recipient of the error value in a catch(err = ...) expression.
	LLVMValueRef error_var;
	LLVMTypeRef bool_type;
	LLVMTypeRef byte_type;
	Decl *cur_code_decl;
	Decl *cur_func_decl;
	TypeInfo *current_return_type;
	int block_global_unique_count;
	int ast_alloca_addr_space;
	BreakContinue return_block;
	int simple_return_expressions;
	unsigned pointer_alignment;
	int return_expressions;
	Ast **defer_stack;
	DebugContext debug;
	Module *code_module;
	LLVMValueRef return_out;
	LLVMValueRef failable_out;
	LLVMBasicBlockRef block_return_exit;
	LLVMBasicBlockRef block_failable_exit;
	LLVMValueRef block_error_var;
	int in_block;
	bool current_block_is_target : 1;
	bool did_call_stack_save : 1;
	LLVMTypeRef type_data_definitions[TYPE_KINDS];
} GenContext;

// LLVM Intrinsics
extern unsigned intrinsic_id_sadd_overflow;
extern unsigned intrinsic_id_sadd_sat;
extern unsigned intrinsic_id_uadd_overflow;
extern unsigned intrinsic_id_uadd_sat;
extern unsigned intrinsic_id_ssub_overflow;
extern unsigned intrinsic_id_ssub_sat;
extern unsigned intrinsic_id_usub_overflow;
extern unsigned intrinsic_id_usub_sat;
extern unsigned intrinsic_id_sshl_sat;
extern unsigned intrinsic_id_ushl_sat;
extern unsigned intrinsic_id_smul_overflow;
extern unsigned intrinsic_id_umul_overflow;
extern unsigned intrinsic_id_trap;
extern unsigned intrinsic_id_bswap;
extern unsigned intrinsic_id_assume;
extern unsigned intrinsic_id_rint;
extern unsigned intrinsic_id_trunc;
extern unsigned intrinsic_id_ceil;
extern unsigned intrinsic_id_sqrt;
extern unsigned intrinsic_id_nearbyint;
extern unsigned intrinsic_id_roundeven;
extern unsigned intrinsic_in_lround;
extern unsigned intrinsic_in_llround;
extern unsigned intrinsic_in_lrint;
extern unsigned intrinsic_in_llrint;
extern unsigned intrinsic_id_floor;
extern unsigned intrinsic_id_powi;
extern unsigned intrinsic_id_pow;
extern unsigned intrinsic_id_sin;
extern unsigned intrinsic_id_cos;
extern unsigned intrinsic_id_exp;
extern unsigned intrinsic_id_exp2;
extern unsigned intrinsic_id_log;
extern unsigned intrinsic_id_log2;
extern unsigned intrinsic_id_log10;
extern unsigned intrinsic_id_fabs;
extern unsigned intrinsic_id_fma;
extern unsigned intrinsic_id_copysign;
extern unsigned intrinsic_id_minnum;
extern unsigned intrinsic_id_maxnum;
extern unsigned intrinsic_id_minimum;
extern unsigned intrinsic_id_maximum;
extern unsigned intrinsic_id_smax;
extern unsigned intrinsic_id_smin;
extern unsigned intrinsic_id_umax;
extern unsigned intrinsic_id_umin;
extern unsigned intrinsic_id_abs;
extern unsigned intrinsic_id_fshl;
extern unsigned intrinsic_id_fshr;
extern unsigned intrinsic_id_bitreverse;
extern unsigned intrinsic_id_bswap;
extern unsigned intrinsic_id_ctpop;
extern unsigned intrinsic_id_ctlz;
extern unsigned intrinsic_id_cttz;
extern unsigned intrinsic_id_convert_from_fp16;
extern unsigned intrinsic_id_convert_to_fp16;
extern unsigned intrinsic_id_lifetime_start;
extern unsigned intrinsic_id_lifetime_end;


// LLVM Attributes
extern unsigned attribute_noinline; // No function inlining
extern unsigned attribute_optnone; // No optimization
extern unsigned attribute_alwaysinline; // Force inlining
extern unsigned attribute_inlinehint; // "Inline possibly"
extern unsigned attribute_noreturn; // No function return
extern unsigned attribute_nounwind; // No exceptions
extern unsigned attribute_writeonly; // No writes on pointer
extern unsigned attribute_readonly; // No reads on pointer
extern unsigned attribute_optnone; // Disable optimization.
extern unsigned attribute_sret; // struct return pointer
extern unsigned attribute_align; // align
extern unsigned attribute_noalias; // noalias (pointer)
extern unsigned attribute_zext; // zero extend
extern unsigned attribute_sext; // sign extend
extern unsigned attribute_byval; // ByVal (param)
extern unsigned attribute_inreg; // inreg (param)
extern unsigned attribute_naked; // naked function

void gencontext_begin_module(GenContext *c);
void gencontext_init_file_emit(GenContext *c, Context *ast);
void gencontext_end_file_emit(GenContext *c, Context *ast);
void gencontext_end_module(GenContext *context);

LLVMValueRef LLVMConstBswap(LLVMValueRef ConstantVal);
#ifndef LLVMCreateTypeAttribute
LLVMAttributeRef LLVMCreateTypeAttribute(LLVMContextRef C, unsigned KindID,
										 LLVMTypeRef type_ref);
#endif

// BE value
void llvm_value_addr(GenContext *c, BEValue *value);
static inline bool llvm_value_is_addr(BEValue *value) { return value->kind == BE_ADDRESS || value->kind == BE_ADDRESS_FAILABLE; }
static inline bool llvm_value_is_bool(BEValue *value) { return value->kind == BE_BOOLEAN; }
bool llvm_value_is_const(BEValue *value);
void llvm_value_rvalue(GenContext *context, BEValue *value);
void llvm_value_set_bool(BEValue *value, LLVMValueRef llvm_value);
void llvm_value_set(BEValue *value, LLVMValueRef llvm_value, Type *type);
void llvm_value_set_int(GenContext *c, BEValue *value, Type *type, uint64_t i);
void llvm_value_set_address_align(BEValue *value, LLVMValueRef llvm_value, Type *type, AlignSize alignment);
void llvm_value_set_address(BEValue *value, LLVMValueRef llvm_value, Type *type);
void llvm_value_set_decl_address(BEValue *value, Decl *decl);
void llvm_value_fold_failable(GenContext *c, BEValue *value);
void llvm_value_struct_gep(GenContext *c, BEValue *element, BEValue *struct_pointer, unsigned index);

LLVMValueRef llvm_value_rvalue_store(GenContext *c, BEValue *value);

LLVMTypeRef llvm_abi_type(GenContext *c, AbiType *type);
TypeSize llvm_abi_size(GenContext *c, LLVMTypeRef type);
BitSize llvm_bitsize(GenContext *c, LLVMTypeRef type);
AlignSize llvm_abi_alignment(GenContext *c, LLVMTypeRef type);
void llvm_attribute_add_range(GenContext *c, LLVMValueRef value_to_add_attribute_to, unsigned attribute_id, int index_start, int index_end);
void llvm_attribute_add(GenContext *c, LLVMValueRef value_to_add_attribute_to, unsigned attribute_id, int index);
void llvm_attribute_add_call(GenContext *context, LLVMValueRef call, unsigned attribute_id, int index, int64_t value);
void llvm_attribute_add_call_type(GenContext *c, LLVMValueRef call, unsigned attribute_id, int index, LLVMTypeRef type);
void llvm_attribute_add_string(GenContext *c, LLVMValueRef value_to_add_attribute_to, const char *attribute, const char *value, int index);
void llvm_attribute_add_type(GenContext *c, LLVMValueRef value_to_add_attribute_to, unsigned attribute_id, LLVMTypeRef type, int index);
void llvm_attribute_add_int(GenContext *c, LLVMValueRef value_to_add_attribute_to, unsigned attribute_id, uint64_t val, int index);
LLVMBasicBlockRef llvm_basic_block_new(GenContext *c, const char *name);
static inline LLVMValueRef llvm_const_int(GenContext *c, Type *type, uint64_t val);
LLVMValueRef llvm_emit_const_padding(GenContext *c, AlignSize size);
LLVMTypeRef llvm_const_padding_type(GenContext *c, TypeSize size);
LLVMValueRef llvm_emit_alloca(GenContext *c, LLVMTypeRef type, unsigned alignment, const char *name);
LLVMValueRef llvm_emit_alloca_aligned(GenContext *c, Type *type, const char *name);
void llvm_emit_and_set_decl_alloca(GenContext *c, Decl *decl);
BEValue llvm_emit_assign_expr(GenContext *context, BEValue *ref, Expr *expr, LLVMValueRef failable);
static inline LLVMValueRef llvm_emit_bitcast(GenContext *context, LLVMValueRef value, Type *type);
void llvm_emit_block(GenContext *c, LLVMBasicBlockRef next_block);
void llvm_emit_br(GenContext *c, LLVMBasicBlockRef next_block);
void llvm_emit_compound_stmt(GenContext *context, Ast *ast);
LLVMValueRef llvm_emit_const_bitstruct(GenContext *c, ConstInitializer *initializer);
void llvm_emit_convert_value_from_coerced(GenContext *c, BEValue *result, LLVMTypeRef coerced, LLVMValueRef value, Type *original_type);
void llvm_emit_coerce_store(GenContext *c, LLVMValueRef addr, AlignSize alignment, LLVMTypeRef coerced, LLVMValueRef value, LLVMTypeRef target_type);
void llvm_emit_function_body(GenContext *context, Decl *decl);
void llvm_emit_function_decl(GenContext *c, Decl *decl);
void gencontext_emit_introspection_type(GenContext *c, Decl *decl);
LLVMValueRef llvm_emit_call_intrinsic(GenContext *c, unsigned intrinsic_id, LLVMTypeRef *types, unsigned type_count, LLVMValueRef *values, unsigned arg_count);
void llvm_emit_cast(GenContext *c, CastKind cast_kind, BEValue *value, Type *to_type, Type *from_type);
void llvm_emit_cond_br(GenContext *context, BEValue *value, LLVMBasicBlockRef then_block, LLVMBasicBlockRef else_block);
void llvm_emit_debug_function(GenContext *c, Decl *decl);
void llvm_emit_debug_location(GenContext *context, SourceSpan location);
void llvm_emit_debug_parameter(GenContext *c, Decl *parameter, unsigned index);
void llvm_emit_debug_local_var(GenContext *c, Decl *var);
void llvm_emit_debug_global_var(GenContext *c, Decl *global);
void llvm_emit_defer(GenContext *c, AstId defer_start, AstId defer_end);
void llvm_emit_extern_decl(GenContext *context, Decl *decl);
LLVMValueRef llvm_emit_const_initializer(GenContext *c, ConstInitializer *const_init);
void llvm_emit_expr(GenContext *c, BEValue *value, Expr *expr);
void llvm_emit_typeid(GenContext *c, BEValue *be_value, Type *type);
void llvm_emit_global_variable_init(GenContext *c, Decl *decl);
void llvm_set_private_linkage(LLVMValueRef alloc);
void llvm_set_internal_linkage(LLVMValueRef alloc);
void llvm_emit_initialize_reference_temporary_const(GenContext *c, BEValue *ref, Expr *expr);
void llvm_emit_int_comp_zero(GenContext *c, BEValue *result, BEValue *lhs, BinaryOp binary_op);
void llvm_emit_int_comparison(GenContext *c, BEValue *result, BEValue *lhs, BEValue *rhs, BinaryOp binary_op);
void llvm_emit_int_comp(GenContext *c, BEValue *result, Type *lhs_type, Type *rhs_type, LLVMValueRef lhs_value, LLVMValueRef rhs_value, BinaryOp binary_op);
void llvm_emit_comparison(GenContext *c, BEValue *be_value, BEValue *lhs, BEValue *rhs, BinaryOp binary_op);
LLVMValueRef llvm_emit_is_no_error_value(GenContext *c, BEValue *value);
void llvm_emit_len_for_expr(GenContext *c, BEValue *be_value, BEValue *expr_to_len);
LLVMValueRef llvm_emit_load_aligned(GenContext *c, LLVMTypeRef type, LLVMValueRef pointer, AlignSize alignment, const char *name);
void llvm_emit_local_var_alloca(GenContext *c, Decl *decl);
LLVMValueRef llvm_emit_local_decl(GenContext *c, Decl *decl);
LLVMValueRef llvm_emit_aggregate_value(GenContext *c, Type *type, ...);
LLVMValueRef llvm_emit_memclear_size_align(GenContext *c, LLVMValueRef ref, uint64_t size, AlignSize align, bool bitcast);
void llvm_emit_memclear(GenContext *c, BEValue *ref);
void llvm_emit_memcpy(GenContext *c, LLVMValueRef dest, unsigned dest_align, LLVMValueRef source, unsigned src_align, uint64_t len);
void llvm_emit_memcpy_to_decl(GenContext *c, Decl *decl, LLVMValueRef source, unsigned source_alignment);
void llvm_emit_stmt(GenContext *c, Ast *ast);
static inline LLVMValueRef llvm_emit_store(GenContext *context, Decl *decl, LLVMValueRef value);
void llvm_emit_panic_on_true(GenContext *c, LLVMValueRef value, const char *panic_name, SourceLocation *loc);
void llvm_emit_panic_if_true(GenContext *c, BEValue *value, const char *panic_name, SourceLocation *loc);
void llvm_emit_ptr_from_array(GenContext *c, BEValue *value);
void llvm_emit_debug_output(GenContext *c, const char *message, const char *file, const char *func, unsigned line);
void llvm_emit_return_abi(GenContext *c, BEValue *return_value, BEValue *failable);
void llvm_emit_return_implicit(GenContext *c);
void llvm_emit_struct_member_ref(GenContext *c, BEValue *struct_ref, BEValue *member_ref, unsigned member_id);
LLVMValueRef llvm_emit_struct_gep_raw(GenContext *context, LLVMValueRef ptr, LLVMTypeRef struct_type, unsigned index,
                                      unsigned struct_alignment, AlignSize *alignment);
LLVMValueRef llvm_emit_array_gep_raw(GenContext *c, LLVMValueRef ptr, LLVMTypeRef array_type, unsigned index, AlignSize array_alignment, AlignSize *alignment);
LLVMValueRef llvm_emit_array_gep_raw_index(GenContext *c, LLVMValueRef ptr, LLVMTypeRef array_type, LLVMValueRef index, AlignSize array_alignment, AlignSize *alignment);
LLVMValueRef llvm_emit_array_load(GenContext *c, LLVMValueRef ptr, LLVMTypeRef array_type, unsigned index, AlignSize array_alignment);
LLVMValueRef llvm_emit_pointer_gep_raw(GenContext *c, LLVMTypeRef pointee_type, LLVMValueRef ptr, LLVMValueRef offset);

LLVMValueRef llvm_emit_pointer_inbounds_gep_raw(GenContext *c, LLVMTypeRef pointee_type, LLVMValueRef ptr, LLVMValueRef offset);

void llvm_emit_subarray_len(GenContext *context, BEValue *subarray, BEValue *len);
void llvm_emit_subarray_pointer(GenContext *context, BEValue *subarray, BEValue *pointer);
LLVMValueRef llvm_get_next_param(GenContext *context, unsigned *index);
LLVMTypeRef llvm_get_coerce_type(GenContext *c, ABIArgInfo *arg_info);
static inline LLVMBasicBlockRef llvm_get_current_block_if_in_use(GenContext *context);
LLVMMetadataRef llvm_get_debug_type(GenContext *c, Type *type);
static inline LLVMTypeRef llvm_get_ptr_type(GenContext *c, Type *type);
LLVMTypeRef llvm_get_type(GenContext *c, Type *any_type);
LLVMTypeRef llvm_get_pointee_type(GenContext *c, Type *any_type);
static inline LLVMValueRef llvm_get_zero(GenContext *c, Type *type);

void llvm_debug_scope_push(GenContext *context, LLVMMetadataRef debug_scope);
void llvm_debug_scope_pop(GenContext *context);
void llvm_debug_push_lexical_scope(GenContext *context, SourceSpan location);
LLVMMetadataRef llvm_debug_current_scope(GenContext *context);


bool llvm_emit_check_block_branch(GenContext *context);


TypeSize llvm_store_size(GenContext *c, LLVMTypeRef type);
LLVMValueRef llvm_store_bevalue(GenContext *c, BEValue *destination, BEValue *value);
void llvm_store_bevalue_raw(GenContext *c, BEValue *destination, LLVMValueRef raw_value);
void llvm_store_bevalue_dest_aligned(GenContext *c, LLVMValueRef destination, BEValue *value);
LLVMValueRef llvm_store_bevalue_aligned(GenContext *c, LLVMValueRef destination, BEValue *value, AlignSize alignment);
void llvm_store_self_aligned(GenContext *context, LLVMValueRef pointer, LLVMValueRef value, Type *type);
LLVMValueRef llvm_store_aligned(GenContext *context, LLVMValueRef pointer, LLVMValueRef value, AlignSize alignment);
void llvm_store_aligned_decl(GenContext *context, Decl *decl, LLVMValueRef value);

LLVMTypeRef llvm_get_twostruct(GenContext *context, LLVMTypeRef lo, LLVMTypeRef hi);
LLVMValueRef llvm_emit_coerce(GenContext *c, LLVMTypeRef coerced, BEValue *value, Type *original_type);

static inline LLVMValueRef gencontext_emit_load(GenContext *c, Type *type, LLVMValueRef value)
{
	assert(llvm_get_type(c, type) == LLVMGetElementType(LLVMTypeOf(value)));
	return LLVMBuildLoad2(c->builder, llvm_get_type(c, type), value, "");
}


static inline LLVMValueRef decl_failable_ref(Decl *decl)
{
	assert(decl->decl_kind == DECL_VAR);
	if (decl->var.kind == VARDECL_UNWRAPPED) return decl_failable_ref(decl->var.alias);
	if (decl->type->type_kind != TYPE_FAILABLE) return NULL;
	return decl->var.failable_ref;
}

static inline LLVMValueRef decl_ref(Decl *decl)
{
	if (decl->decl_kind == DECL_VAR && decl->var.kind == VARDECL_UNWRAPPED) return decl_ref(decl->var.alias);
	return decl->backend_ref;
}

static inline LLVMValueRef llvm_emit_store(GenContext *context, Decl *decl, LLVMValueRef value)
{
	return LLVMBuildStore(context->builder, value, decl_ref(decl));
}

static inline LLVMValueRef llvm_emit_bitcast(GenContext *context, LLVMValueRef value, Type *type)
{
	assert(type->type_kind == TYPE_POINTER);
	LLVMTypeRef result_type = llvm_get_type(context, type);
	if (result_type == LLVMTypeOf(value)) return value;
	if (LLVMIsConstant(value))
	{
		return LLVMConstBitCast(value, result_type);
	}
	assert(context->builder);
	return LLVMBuildBitCast(context->builder, value, result_type, "");
}

static inline bool llvm_use_debug(GenContext *context) { return context->debug.builder != NULL; }

static inline bool llvm_basic_block_is_unused(LLVMBasicBlockRef block)
{
	return !LLVMGetFirstInstruction(block) && !LLVMGetFirstUse(LLVMBasicBlockAsValue(block));
}

static inline LLVMBasicBlockRef llvm_get_current_block_if_in_use(GenContext *context)
{
	LLVMBasicBlockRef block = context->current_block;
	if (llvm_basic_block_is_unused(block))
	{
		LLVMDeleteBasicBlock(block);
		context->current_block = NULL;
		context->current_block_is_target = false;
		return NULL;
	}
	return block;
}

static inline bool call_supports_variadic(CallABI abi)
{
	switch (abi)
	{
		case CALL_X86_STD:
		case CALL_X86_REG:
		case CALL_X86_THIS:
		case CALL_X86_FAST:
		case CALL_X86_VECTOR:
			return false;
		default:
			return true;

	}
}

static inline LLVMCallConv llvm_call_convention_from_call(CallABI abi, ArchType arch, OsType os)
{
	switch (abi)
	{
		case CALL_C:
			return LLVMCCallConv;
		case CALL_X86_STD:
			return LLVMX86StdcallCallConv;
		case CALL_X86_FAST:
			return LLVMX86FastcallCallConv;
		case CALL_X86_REG:
			return LLVMX86RegCallCallConv;
		case CALL_X86_THIS:
			return LLVMX86ThisCallCallConv;
		case CALL_X86_VECTOR:
			return LLVMX86VectorCallCallConv;
		case CALL_AAPCS:
			return LLVMARMAAPCSCallConv;
		case CALL_AAPCS_VFP:
			return LLVMARMAAPCSVFPCallConv;
		default:
			return LLVMCCallConv;
	}

}


static inline LLVMTypeRef llvm_get_ptr_type(GenContext *c, Type *type)
{
	return llvm_get_type(c, type_get_ptr(type));
}

static inline LLVMValueRef llvm_get_zero(GenContext *c, Type *type)
{
	return LLVMConstNull(llvm_get_type(c, type));
}

static inline LLVMValueRef llvm_const_int(GenContext *c, Type *type, uint64_t val)
{
	type = type_lowering(type);
	assert(type_is_integer(type) || type->type_kind == TYPE_BOOL);
	return LLVMConstInt(llvm_get_type(c, type), val, type_is_integer_signed(type));
}

static inline void llvm_set_alignment(LLVMValueRef alloca, AlignSize alignment)
{
	assert(alignment > 0);
	LLVMSetAlignment(alloca, (unsigned)alignment);
}
void llvm_set_error_exit(GenContext *c, LLVMBasicBlockRef block);
void llvm_set_error_exit_and_value(GenContext *c, LLVMBasicBlockRef block, LLVMValueRef value);

#define EMIT_LOC(c, x) do { if (c->debug.builder) llvm_emit_debug_location(c, x->span); } while (0);

#define PUSH_ERROR() \
 LLVMBasicBlockRef _old_catch = c->catch_block; \
 LLVMValueRef _old_error_var = c->error_var
#define POP_ERROR() \
 c->catch_block = _old_catch; \
 c->error_var = _old_error_var


static inline bool abi_info_should_flatten(ABIArgInfo *info);

static inline bool abi_info_should_flatten(ABIArgInfo *info)
{
	return info->kind == ABI_ARG_DIRECT_COERCE && info->direct_coerce.elements > 1U && !info->direct_coerce.prevent_flatten;
}

