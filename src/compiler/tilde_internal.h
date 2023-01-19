#pragma once

#include "codegen_internal.h"

// Copyright (c) 2022 Christoffer Lerno. All rights reserved.
// Use of this source code is governed by a LGPLv3.0
// a copy of which can be found in the LICENSE file.

#undef TB_Reg
#include <tb.h>


typedef enum
{
	TBE_VALUE,
	TBE_ADDRESS,
	TBE_ADDRESS_OPTIONAL,
} TBBackendValueKind;

typedef struct
{
	TBBackendValueKind kind: 5;
	AlignSize alignment;

	Type *type; // Should never be a distinct or canonical type.

	TB_Reg reg;
	TB_Reg failable;
} TBEValue;

typedef struct
{
	Module *code_module;
	const char *object_filename;
	const char *ir_filename;
	const char *asm_filename;

	TB_Function **functions;
	TB_Module *module;
	TB_FeatureSet features;

	struct
	{
		const char *name;
		FunctionPrototype *prototype;
		Type *rtype;
	} cur_func;
	Decl *curr_func;
	TB_Function *f;
	TB_Reg opt_var;
	TB_Label catch_block;
	TBEValue retval;
	TB_Reg return_out;
	TB_Reg optional_out;

	struct
	{
		TB_Reg last_ptr;
		TB_Reg stack_slot;
		CompilationUnit *compile_unit;
	} debug;
} TildeContext;




#define PUSH_OPT() TB_Label _old_catch = c->catch_block; TB_Reg _old_opt_var = c->opt_var
#define POP_OPT() c->catch_block = _old_catch; c->opt_var = _old_opt_var


// -- store ---
static inline void tilde_store_internal(TildeContext *c, TB_DataType type, TB_Reg addr, TB_Reg value, AlignSize alignment);
void tilde_store_to_ptr_raw_aligned(TildeContext *c, Type *type, TB_Reg addr, TB_Reg value, AlignSize alignment);
void tilde_store_to_ptr_raw(TildeContext *c, TB_Reg addr, TB_Reg value, Type *type);
void tilde_store_value_raw(TildeContext *c, TBEValue *destination, TB_Reg value);
void tilde_store_to_ptr_aligned(TildeContext *c, TB_Reg destination, TBEValue *value, AlignSize alignment);
void tilde_store_value_aligned(TildeContext *c, TB_Reg destination, TBEValue *value, AlignSize alignment);
void tilde_store(TildeContext *c, TBEValue *dst, TBEValue *value);
void tilde_store_decl_raw(TildeContext *c, Decl *decl, TB_Reg value);
TB_Reg tilde_load_natural_alignment(TildeContext *c, Type *type, TB_Reg pointer);
TB_Reg tilde_load_value_store(TildeContext *c, TBEValue *value);
void tilde_store_zero(TildeContext *c, Type *type, TB_Reg addr, AlignSize alignment);
void tilde_store_value_zero(TildeContext *c, TBEValue *to);
INLINE void tilde_store_to_ptr(TildeContext *c, TB_Reg destination, TBEValue *value);

// -- load ---
static inline TB_Reg tilde_load(TildeContext *c, TB_DataType type, TB_Reg addr, AlignSize alignment);
TB_Reg tilde_load_abi_alignment(TildeContext *c, Type *type, TB_Reg pointer);
TB_Reg tilde_load_value(TildeContext *c, TBEValue *value);

// -- value --
void value_set(TBEValue *value, TB_Reg val, Type *type);
void value_set_address(TBEValue *value, TB_Reg addr, Type *type, AlignSize alignment);
void value_set_address_abi_aligned(TBEValue *value, TB_Reg val, Type *type);
void value_set_decl_address(TildeContext *c, TBEValue *value, Decl *decl);
void value_set_decl(TildeContext *c, TBEValue *value, Decl *decl);
void value_addr(TildeContext *c, TBEValue *value);

static inline bool value_is_addr(TBEValue *value) { return value->kind == TBE_ADDRESS || value->kind == TBE_ADDRESS_OPTIONAL; }
void value_fold_optional(TildeContext *c, TBEValue *value);
void value_rvalue(TildeContext *c, TBEValue *value);

TB_Function *tilde_get_function(TildeContext *c, Decl *decl);
TB_Reg tilde_get_ref(TildeContext *c, Decl *decl);
TB_Reg tilde_get_opt_ref(TildeContext *c, Decl *decl);
TB_Reg tilde_get_const_int(TildeContext *c, Type *type, uint64_t i);
TB_Reg tilde_get_const_float(TildeContext *c, Type *type, double d);
TB_Register tilde_get_zero(TildeContext *c, Type *type);

// -- type ---
TB_DataType tilde_abi_type(AbiType type);
TB_DataType tildetype(Type *type);
TB_Global *tilde_get_typeid(TildeContext *c, Type *type);
TB_DataType tilde_update_prototype_abi(TildeContext *context, FunctionPrototype *prototype, TB_DataType **params);
void tilde_emit_function_decl(TildeContext *c, Decl *decl);

// -- instructions --
void tilde_emit_cond_br(TildeContext *c, TBEValue *value, TB_Label then_block, TB_Label else_block);
TB_Reg tilde_emit_lshr_fixed(TildeContext *c, Type *type, TB_Reg reg, int shift);

// -- stmt ---
void tilde_emit_stmt(TildeContext *c, Ast *ast);
void tilde_emit_return_implicit(TildeContext *c);

// -- general ---
TB_Register tilde_emit_is_no_error(TildeContext *c, TB_Reg reg);
void tilde_emit_global_initializer(TildeContext *c, Decl *decl);
TB_Reg tilde_emit_alloca(TildeContext *c, Type *type, AlignSize alignment);
void tilde_emit_local_var_alloca(TildeContext *c, Decl *decl);
void tilde_emit_and_set_decl_alloca(TildeContext *c, Decl *decl);
INLINE TB_Linkage tilde_linkage_for_decl(Decl *decl);
void tilde_emit_parameter(TildeContext *c, TB_Reg *args, unsigned *arg_count_ref, ABIArgInfo *info, TBEValue *be_value, Type *type);
INLINE bool tilde_use_debug(TildeContext *context);


// -- expr --
void tilde_emit_expr(TildeContext *c, TBEValue *result, Expr *expr);
TBEValue tilde_emit_assign_expr(TildeContext *c, TBEValue *ref, Expr *expr, TB_Reg optional);
void tilde_emit_cast(TildeContext *c, CastKind cast_kind, Expr *expr, TBEValue *value, Type *to_type, Type *from_type);

// -- comparisons --
void tilde_emit_comp(TildeContext *c, TBEValue *result, TBEValue *lhs, TBEValue *rhs, BinaryOp binary_op);
void tilde_emit_int_comp(TildeContext *c, TBEValue *result, TBEValue *lhs, TBEValue *rhs, BinaryOp binary_op);
void tilde_emit_int_comp_zero(TildeContext *c, TBEValue *result, TBEValue *lhs, BinaryOp binary_op);
void tilde_emit_int_comp_raw(TildeContext *c, TBEValue *result, Type *lhs_type, Type *rhs_type, TB_Reg lhs_value, TB_Reg rhs_value, BinaryOp binary_op);

// -- optional --
void tilde_emit_jump_to_optional_exit(TildeContext *c, TB_Reg opt_value);
TB_Reg tilde_emit_is_no_opt(TildeContext *c, TB_Reg error_value);


// -- jumps ---
bool tilde_emit_goto_if_needed(TildeContext *c, TB_Label jump);
INLINE void tilde_emit_block(TildeContext *c, TB_Label block);

void tilde_emit_memclear_size_align(TildeContext *c, TB_Register ref, uint64_t size, AlignSize align);

#define EMIT_LOC(x, y) do { } while(0)


static inline void tilde_store_internal(TildeContext *c, TB_DataType type, TB_Reg addr, TB_Reg value, AlignSize alignment)
{
	assert(alignment > 0);
	tb_inst_store(c->f, type, addr, value, alignment);
}

static inline TB_Reg tilde_load(TildeContext *c, TB_DataType type, TB_Reg addr, AlignSize alignment)
{
	assert(alignment > 0);
	return tb_inst_load(c->f, type, addr, alignment);
}

static inline TB_Reg decl_reg(Decl *decl)
{
	if (decl->decl_kind == DECL_VAR && decl->var.kind == VARDECL_UNWRAPPED) return decl_reg(decl->var.alias);
	assert(!decl->is_value);
	return decl->tb_register;
}

TB_DataType tilde_get_int_type_of_bytesize(int byte_size);
TB_FunctionPrototype *tilde_get_func_prototype(TildeContext *c, FunctionPrototype *prototype);
INLINE TB_Linkage tilde_linkage_for_decl(Decl *decl)
{
	if (!decl->is_external_visible && decl->visibility == VISIBLE_LOCAL) return TB_LINKAGE_PRIVATE;
	return TB_LINKAGE_PUBLIC;
}

INLINE void tilde_store_to_ptr(TildeContext *c, TB_Reg destination, TBEValue *value)
{
	return tilde_store_to_ptr_aligned(c, destination, value, type_alloca_alignment(value->type));
}

INLINE void tilde_emit_block(TildeContext *c, TB_Label block)
{
	tb_inst_set_label(c->f, block);
}

INLINE bool tilde_use_debug(TildeContext *context)
{
	return false;
}