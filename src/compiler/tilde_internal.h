#pragma once

#include "codegen_internal.h"

// Copyright (c) 2022 Christoffer Lerno. All rights reserved.
// Use of this source code is governed by a LGPLv3.0
// a copy of which can be found in the LICENSE file.

#if TB_BACKEND

#include <tb.h>

typedef struct
{
	Module *code_module;
	const char *object_filename;

	TB_Module *module;
	TB_FeatureSet features;

	Decl *current_func_decl;
	TB_Function *f;
	TB_Register error_var;
	TB_Label catch_block;
} TbContext;

typedef enum
{
	TBE_VALUE,
	TBE_ADDRESS,
	TBE_ADDRESS_FAILABLE,
} TBBackendValueKind;

typedef struct
{
	TBBackendValueKind kind: 5;
	AlignSize alignment;

	Type *type; // Should never be a distinct or canonical type.

	TB_Register reg;
	TB_Register failable; // TODO what even is a failable?
} TBEValue;

TB_DataType tbtype(Type *type);

#define PUSH_ERROR() \
TB_Label _old_catch = c->catch_block; \
TB_Reg _old_error_var = c->error_var
#define POP_ERROR() \
c->catch_block = _old_catch; \
c->error_var = _old_error_var

// -- store ---
static inline void tilde_store(TbContext *c, TB_DataType type, TB_Reg addr, TB_Reg value, AlignSize alignment);
void tilde_store_raw(TbContext *c, Type *type, TB_Reg addr, TB_Reg value, AlignSize alignment);
void tilde_store_raw_abi_alignment(TbContext *c, Type *type, TB_Reg addr, TB_Reg value);
void tilde_store_value_raw(TbContext *c, TBEValue *destination, TB_Reg value);
void tilde_store_value_aligned(TbContext *c, TB_Reg destination, TBEValue *value, AlignSize alignment);
void tilde_store_value(TbContext *c, TBEValue *dst, TBEValue *value);
void tilde_store_decl_raw(TbContext *c, Decl *decl, TB_Reg value);
void tilde_store_zero(TbContext *c, Type *type, TB_Reg addr, AlignSize alignment);
void tilde_store_value_zero(TbContext *c, TBEValue *to);

// -- load ---
static inline TB_Reg tilde_load(TbContext *c, TB_DataType type, TB_Reg addr, AlignSize alignment);
TB_Reg tilde_load_abi_alignment(TbContext *c, Type *type, TB_Reg pointer);
TB_Reg tilde_load_value(TbContext *c, TBEValue *value);

// -- value --
void value_set(TBEValue *value, TB_Reg val, Type *type);
void value_set_address(TBEValue *value, TB_Reg addr, Type *type, AlignSize alignment);
void value_set_address_abi_aligned(TBEValue *value, TB_Reg val, Type *type);
void value_set_decl(TBEValue *value, Decl *decl);
void value_addr(TbContext *c, TBEValue *value);

static inline bool value_is_addr(TBEValue *value) { return value->kind == TBE_ADDRESS || value->kind == TBE_ADDRESS_FAILABLE; }
void value_fold_failable(TbContext *c, TBEValue *value);
void value_rvalue(TbContext *c, TBEValue *value);


TB_Register tilde_get_zero(TbContext *c, Type *type);

// -- instructions --
void tilde_emit_cond_br(TbContext *c, TBEValue *value, TB_Label then_block, TB_Label else_block);
TB_Reg tilde_emit_lshr_fixed(TbContext *c, Type *type, TB_Reg reg, int shift);
TB_Reg tilde_emit_alloca(TbContext *c, Type *type);

// -- stmt ---
void tilde_emit_stmt(TbContext *c, Ast *ast);
void tilde_emit_defer(TbContext *c, AstId defer_start, AstId defer_end);

// -- general ---
TB_Register tilde_emit_is_no_error(TbContext *c, TB_Reg reg);
void tilde_emit_global_initializer(TbContext *c, Decl *decl);
void tilde_emit_local_var_alloca(TbContext *c, Decl *decl);
void tilde_emit_and_set_decl_alloca(TbContext *c, Decl *decl);

// -- expr --
void tilde_emit_expr(TbContext *c, TBEValue *result, Expr *expr);
TBEValue tilde_emit_assign_expr(TbContext *c, TBEValue *ref, Expr *expr, TB_Reg failable);

void tilde_emit_memclear_size_align(TbContext *c, TB_Register ref, uint64_t size, AlignSize align);


static inline void tilde_store(TbContext *c, TB_DataType type, TB_Reg addr, TB_Reg value, AlignSize alignment)
{
	assert(alignment > 0);
	tb_inst_store(c->f, type, addr, value, alignment);
}

static inline TB_Reg tilde_load(TbContext *c, TB_DataType type, TB_Reg addr, AlignSize alignment)
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

#endif // TB_BACKEND