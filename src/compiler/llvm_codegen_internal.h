#pragma once

// Copyright (c) 2019 Christoffer Lerno. All rights reserved.
// Use of this source code is governed by the GNU LGPLv3.0 license
// a copy of which can be found in the LICENSE file.

#include "compiler_internal.h"
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
	unsigned alignment : 16;
	Type *type;
	LLVMValueRef value;
	LLVMValueRef failable;
} BEValue;

typedef struct
{
	LLVMBasicBlockRef continue_block;
	LLVMBasicBlockRef break_block;
	LLVMBasicBlockRef next_block;
}  BreakContinue;

typedef enum
{
	ABI_ARG_IGNORE,
	ABI_ARG_DIRECT_PAIR,
	ABI_ARG_DIRECT_COERCE,
	ABI_ARG_EXPAND_COERCE,
	ABI_ARG_INDIRECT,
	ABI_ARG_EXPAND,
}  ABIKind;

typedef enum
{
	ABI_TYPE_PLAIN,
	ABI_TYPE_INT_BITS
} AbiTypeKind;

typedef struct
{
	AbiTypeKind kind : 2;
	union
	{
		Type *type;
		unsigned int_bits;
	};
} AbiType;

typedef struct ABIArgInfo_
{
	unsigned param_index_start : 16;
	unsigned param_index_end : 16;
	ABIKind kind : 6;
	struct
	{
		bool by_reg : 1;
		bool zeroext : 1;
		bool signext : 1;
	} attributes;
	union
	{
		struct
		{
			bool padding_by_reg : 1;
			Type *padding_type;
		} expand;
		struct
		{
			AbiType *lo;
			AbiType *hi;
		} direct_pair;
		struct
		{
			unsigned char offset_lo;
			unsigned char padding_hi;
			unsigned char lo_index;
			unsigned char hi_index;
			unsigned char offset_hi;
			bool packed : 1;
			AbiType *lo;
			AbiType *hi;
		} coerce_expand;
		struct
		{
			AbiType *partial_type;
		};
		struct
		{
			AbiType *type;
			unsigned elements : 3;
			bool prevent_flatten : 1;
		} direct_coerce;
		struct
		{
			// We may request a certain alignment of the parameters.
			unsigned realignment : 16;
			bool by_val : 1;
		} indirect;
	};

} ABIArgInfo;


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
	LLVMContextRef context;
	LLVMValueRef function;
	LLVMValueRef alloca_point;
	LLVMBuilderRef builder;
	LLVMBasicBlockRef current_block;
	LLVMBasicBlockRef catch_block;
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
	Context *ast_context;
	LLVMValueRef return_out;
	LLVMValueRef failable_out;
	LLVMBasicBlockRef error_exit_block;
	LLVMBasicBlockRef expr_block_exit;
	bool current_block_is_target : 1;
	bool did_call_stack_save : 1;
	LLVMTypeRef type_data_definitions[TYPE_KINDS];
	struct
	{
		unsigned int_registers;
		unsigned sse_registers;
		unsigned simd_registers;
		int args;
		CallConvention call_convention;
	} abi;
} GenContext;

// LLVM Intrinsics
extern unsigned intrinsic_id_sadd_overflow;
extern unsigned intrinsic_id_uadd_overflow;
extern unsigned intrinsic_id_ssub_overflow;
extern unsigned intrinsic_id_usub_overflow;
extern unsigned intrinsic_id_smul_overflow;
extern unsigned intrinsic_id_umul_overflow;
extern unsigned intrinsic_id_trap;
extern unsigned intrinsic_id_assume;

// LLVM Attributes
extern unsigned attribute_noinline; // No function inlining
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

void gencontext_begin_module(GenContext *context);
void gencontext_end_module(GenContext *context);

// BE value
void llvm_value_addr(GenContext *c, BEValue *value);
static inline bool llvm_value_is_addr(BEValue *value) { return value->kind == BE_ADDRESS || value->kind == BE_ADDRESS_FAILABLE; }
static inline bool llvm_value_is_bool(BEValue *value) { return value->kind == BE_BOOLEAN; }
bool llvm_value_is_const(BEValue *value);
void llvm_value_rvalue(GenContext *context, BEValue *value);
void llvm_value_set_bool(BEValue *value, LLVMValueRef llvm_value);
void llvm_value_set(BEValue *value, LLVMValueRef llvm_value, Type *type);
void llvm_value_set_address_align(BEValue *value, LLVMValueRef llvm_value, Type *type, unsigned alignment);
void llvm_value_set_address(BEValue *value, LLVMValueRef llvm_value, Type *type);
void llvm_value_fold_failable(GenContext *c, BEValue *value);
void llvm_value_struct_gep(GenContext *c, BEValue *element, BEValue *struct_pointer, unsigned index);

LLVMValueRef llvm_value_rvalue_store(GenContext *c, BEValue *value);

LLVMTypeRef llvm_abi_type(GenContext *c, AbiType *type);
unsigned llvm_abi_size(LLVMTypeRef type);
unsigned llvm_abi_alignment(LLVMTypeRef type);
void llvm_attribute_add_range(GenContext *c, LLVMValueRef value_to_add_attribute_to, unsigned attribute_id, int index_start, int index_end);
void llvm_attribute_add(GenContext *c, LLVMValueRef value_to_add_attribute_to, unsigned attribute_id, int index);
void llvm_attribute_add_string(GenContext *c, LLVMValueRef value_to_add_attribute_to, const char *attribute, const char *value, int index);
void llvm_attribute_add_int(GenContext *c, LLVMValueRef value_to_add_attribute_to, unsigned attribute_id, uint64_t val, int index);
LLVMBasicBlockRef llvm_basic_block_new(GenContext *c, const char *name);
static inline LLVMValueRef llvm_const_int(GenContext *c, Type *type, uint64_t val);

LLVMValueRef llvm_emit_alloca(GenContext *context, LLVMTypeRef type, unsigned alignment, const char *name);
LLVMValueRef llvm_emit_alloca_aligned(GenContext *c, Type *type, const char *name);
LLVMValueRef llvm_emit_assign_expr(GenContext *context, LLVMValueRef ref, Expr *expr, LLVMValueRef failable);
static inline LLVMValueRef llvm_emit_bitcast(GenContext *context, LLVMValueRef value, Type *type);
void llvm_emit_block(GenContext *c, LLVMBasicBlockRef next_block);
void llvm_emit_br(GenContext *c, LLVMBasicBlockRef next_block);
void llvm_emit_compound_stmt(GenContext *context, Ast *ast);
LLVMValueRef llvm_emit_convert_value_from_coerced(GenContext *context, LLVMTypeRef coerced, LLVMValueRef value, Type *original_type);
LLVMValueRef llvm_emit_decl_alloca(GenContext *c, Decl *decl);
void llvm_emit_function_body(GenContext *context, Decl *decl);
void llvm_emit_function_decl(GenContext *c, Decl *decl);
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
LLVMValueRef llvm_emit_is_no_error(GenContext *c, LLVMValueRef error);
LLVMValueRef llvm_emit_load_aligned(GenContext *c, LLVMTypeRef type, LLVMValueRef pointer, unsigned alignment, const char *name);
void llvm_emit_expr(GenContext *c, BEValue *value, Expr *expr);
LLVMValueRef llvm_emit_memclear_size_align(GenContext *c, LLVMValueRef ref, uint64_t size, unsigned align, bool bitcast);
LLVMValueRef llvm_emit_memclear(GenContext *c, LLVMValueRef ref, Type *type);
void llvm_emit_memcpy_to_decl(GenContext *c, Decl *decl, LLVMValueRef source, unsigned source_alignment);
void llvm_emit_stmt(GenContext *c, Ast *ast);
static inline LLVMValueRef llvm_emit_store(GenContext *context, Decl *decl, LLVMValueRef value);
void llvm_emit_panic_on_true(GenContext *c, LLVMValueRef value, const char *panic_name);
void llvm_emit_return_abi(GenContext *c, BEValue *return_value, BEValue *failable);
void llvm_emit_return_implicit(GenContext *c);
LLVMValueRef llvm_emit_struct_gep(GenContext *context, LLVMValueRef ptr, LLVMTypeRef struct_type, unsigned index, unsigned struct_alignment, unsigned offset, unsigned *alignment);

LLVMValueRef llvm_get_next_param(GenContext *context, unsigned *index);
LLVMTypeRef llvm_get_coerce_type(GenContext *c, ABIArgInfo *arg_info);
static inline LLVMBasicBlockRef llvm_get_current_block_if_in_use(GenContext *context);
LLVMMetadataRef llvm_get_debug_type(GenContext *c, Type *type);
static inline LLVMTypeRef llvm_get_ptr_type(GenContext *c, Type *type);
LLVMTypeRef llvm_get_type(GenContext *c, Type *any_type);
static inline LLVMValueRef llvm_get_zero(GenContext *c, Type *type);

void llvm_debug_scope_push(GenContext *context, LLVMMetadataRef debug_scope);
void llvm_debug_scope_pop(GenContext *context);
void llvm_debug_push_lexical_scope(GenContext *context, SourceSpan location);
LLVMMetadataRef llvm_debug_current_scope(GenContext *context);

void c_abi_func_create(GenContext *context, FunctionSignature *signature);

bool llvm_emit_check_block_branch(GenContext *context);


unsigned llvm_store_size(LLVMTypeRef type);
void llvm_store_bevalue(GenContext *c, BEValue *destination, BEValue *value);
void llvm_store_bevalue_raw(GenContext *c, BEValue *destination, LLVMValueRef raw_value);
void llvm_store_bevalue_dest_aligned(GenContext *c, LLVMValueRef destination, BEValue *value);
void llvm_store_bevalue_aligned(GenContext *c, LLVMValueRef destination, BEValue *value, unsigned alignment);
void llvm_store_self_aligned(GenContext *context, LLVMValueRef pointer, LLVMValueRef value, Type *type);
void llvm_store_aligned(GenContext *context, LLVMValueRef pointer, LLVMValueRef value, unsigned alignment);
void llvm_store_aligned_decl(GenContext *context, Decl *decl, LLVMValueRef value);

LLVMTypeRef llvm_get_twostruct(GenContext *context, LLVMTypeRef lo, LLVMTypeRef hi);
LLVMValueRef llvm_emit_coerce(GenContext *context, LLVMTypeRef coerced, BEValue *value, Type *original_type);

static inline LLVMValueRef gencontext_emit_load(GenContext *c, Type *type, LLVMValueRef value)
{
	assert(llvm_get_type(c, type) == LLVMGetElementType(LLVMTypeOf(value)));
	return LLVMBuildLoad2(c->builder, llvm_get_type(c, type), value, "");
}


static inline LLVMValueRef decl_failable_ref(Decl *decl)
{
	assert(decl->decl_kind == DECL_VAR);
	if (decl->var.kind == VARDECL_ALIAS) return decl_failable_ref(decl->var.alias);
	if (!decl->var.failable) return NULL;
	return decl->var.failable_ref;
}

static inline LLVMValueRef decl_ref(Decl *decl)
{
	if (decl->decl_kind == DECL_VAR && decl->var.kind == VARDECL_ALIAS) return decl_ref(decl->var.alias);
	return decl->backend_ref;
}

static inline LLVMValueRef llvm_emit_store(GenContext *context, Decl *decl, LLVMValueRef value)
{
	return LLVMBuildStore(context->builder, value, decl_ref(decl));
}

static inline LLVMValueRef llvm_emit_bitcast(GenContext *context, LLVMValueRef value, Type *type)
{
	return LLVMBuildBitCast(context->builder, value, llvm_get_type(context, type), "");
}

static inline bool llvm_use_debug(GenContext *context) { return context->debug.builder != NULL; }

static inline LLVMBasicBlockRef llvm_get_current_block_if_in_use(GenContext *context)
{
	LLVMBasicBlockRef block = context->current_block;
	if (!LLVMGetFirstInstruction(block) && !LLVMGetFirstUse(LLVMBasicBlockAsValue(block)))
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
		case CALL_X86_PASCAL:
		case CALL_X86_VECTOR:
		case CALL_SPIR_FUNCTION:
		case CALL_OPENCL_KERNEL:
			return false;
		default:
			return true;

	}
}

static inline LLVMCallConv llvm_call_convention_from_call(CallABI abi)
{
	switch (abi)
	{
		case CALL_X86_STD:
			return LLVMX86StdcallCallConv;
		case CALL_X86_FAST:
			return LLVMX86FastcallCallConv;
		case CALL_X86_PASCAL:
			return LLVMCCallConv;
		case CALL_X86_REG:
			return LLVMX86RegCallCallConv;
		case CALL_X86_THIS:
			return LLVMX86ThisCallCallConv;
		case CALL_X86_VECTOR:
			return LLVMX86VectorCallCallConv;
		case CALL_WIN64:
			return LLVMWin64CallConv;
		case CALL_X64_SYSV:
			return LLVMX8664SysVCallConv;
		case CALL_AAPCS:
			return LLVMARMAAPCSCallConv;
		case CALL_AAPCS_VFP:
			return LLVMARMAAPCSVFPCallConv;
		case CALL_INTEL_OCL_BICC:
			return LLVMIntelOCLBICallConv;
		case CALL_AARCH64_VECTOR:
			TODO
		case CALL_SPIR_FUNCTION:
			return LLVMSPIRFUNCCallConv;
		case CALL_OPENCL_KERNEL:
			TODO // Target dependent.
		case CALL_PRESERVE_ALL:
			return LLVMPreserveAllCallConv;
		case CALL_PRESERVE_MOST:
			return LLVMPreserveMostCallConv;
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
	type = type->canonical;
	assert(type_is_any_integer(type) || type->type_kind == TYPE_BOOL);
	return LLVMConstInt(llvm_get_type(c, type), val, type_is_integer_signed(type));
}


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
