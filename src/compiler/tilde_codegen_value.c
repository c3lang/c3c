#include "tilde_internal.h"


void value_set(TBEValue *value, TB_Reg val, Type *type)
{
	type = type_lowering(type);
	assert(val || type == type_void);
	value->reg = val;
	value->alignment = type_abi_alignment(type);
	value->kind = TBE_VALUE;
	value->type = type;
}

void value_set_decl(TildeContext *c, TBEValue *value, Decl *decl)
{
	decl = decl_flatten(decl);
	if (decl->is_value)
	{
		value_set(value, decl->tb_register, decl->type);
		return;
	}
	value_set_decl_address(c, value, decl);
}


void value_set_address(TBEValue *value, TB_Reg addr, Type *type, AlignSize alignment)
{
	value->reg = addr;
	value->alignment = alignment;
	value->kind = TBE_ADDRESS;
	value->type = type_lowering(type);
}

void value_set_address_abi_aligned(TBEValue *value, TB_Reg val, Type *type)
{
	value_set_address(value, val, type, type_abi_alignment(type));
}

void value_addr(TildeContext *c, TBEValue *value)
{
	value_fold_optional(c, value);
	if (value->kind == TBE_ADDRESS) return;
	if (!c->f)
	{
		TODO
	}
	else
	{
		TODO
	}
}

TB_Reg tilde_get_opt_ref(TildeContext *c, Decl *decl)
{
	tilde_get_ref(c, decl);
	decl = decl_flatten(decl);
	if (decl->decl_kind != DECL_VAR) return TB_NULL_REG;
	return decl->var.tb_optional_reg;
}

TB_Function *tilde_get_function(TildeContext *c, Decl *decl)
{
	assert(decl->decl_kind == DECL_FUNC);
	if (decl->tb_symbol && ((TB_Symbol *)decl->tb_symbol)->module == c->module)
	{
		return decl->tb_symbol;
	}
	bool is_internal = decl->unit->module == c->code_module && !decl->is_external_visible && !visible_external(decl->visibility);
	decl->tb_symbol = tb_function_create(c->module, decl_get_extname(decl), is_internal ? TB_LINKAGE_PRIVATE : TB_LINKAGE_PUBLIC);
	tb_function_set_prototype(decl->tb_symbol, tilde_get_func_prototype(c, decl->type->function.prototype));
	return decl->tb_symbol;
}

TB_Reg tilde_get_ref(TildeContext *c, Decl *decl)
{
	switch (decl->decl_kind)
	{
		case DECL_VAR:
			if (decl_is_local(decl))
			{
				return decl->tb_register;
			}
			if (decl->var.kind == VARDECL_UNWRAPPED) return tilde_get_ref(c, decl->var.alias);
			assert(decl->var.kind == VARDECL_GLOBAL || decl->var.kind == VARDECL_CONST);
			{
				TB_Symbol *symbol = decl->backend_value;
				if (symbol->module != c->module || !symbol)
				{
					TODO
				}
				return tb_inst_get_symbol_address(c->f, decl->backend_value);
			}
		case DECL_FUNC:
			/*
			backend_ref = decl->backend_ref = LLVMAddFunction(c->module, decl_get_extname(decl), llvm_get_type(c, decl->type));
			if (decl->unit->module == c->code_module && !decl->is_external_visible && !visible_external(decl->visibility))
			{
				llvm_set_internal_linkage(backend_ref);
			}
			return backend_ref;*/
		case DECL_DEFINE:
			if (decl->define_decl.define_kind != DEFINE_TYPE_GENERIC) return tilde_get_ref(c, decl->define_decl.alias);
			UNREACHABLE
		case DECL_FAULTVALUE:
			/*
			if (!decl->backend_ref)
			{
				llvm_get_typeid(c, declptr(decl->enum_constant.parent)->type);
			}
			assert(decl->backend_ref);
			return decl->backend_ref;*/
			TODO
		case DECL_POISONED:
		case DECL_ATTRIBUTE:
		case DECL_BITSTRUCT:
		case DECL_CT_CASE:
		case DECL_CT_ELIF:
		case DECL_CT_ELSE:
		case DECL_CT_IF:
		case DECL_CT_SWITCH:
		case DECL_CT_ASSERT:
		case DECL_DISTINCT:
		case DECL_ENUM:
		case DECL_ENUM_CONSTANT:
		case DECL_FAULT:
		case DECL_GENERIC:
		case DECL_IMPORT:
		case DECL_LABEL:
		case DECL_MACRO:
		case DECL_STRUCT:
		case DECL_TYPEDEF:
		case DECL_UNION:
		case DECL_DECLARRAY:
		case DECL_INITIALIZE:
		case DECL_FINALIZE:
		case DECL_BODYPARAM:
		case DECL_CT_ECHO:
		case DECL_CT_INCLUDE:
			UNREACHABLE;
	}
	UNREACHABLE
}


void value_set_decl_address(TildeContext *c, TBEValue *value, Decl *decl)
{
	TB_Reg backend_ref = tilde_get_ref(c, decl);
	value_set_address(value, backend_ref, decl->type, decl->alignment);

	if ((value->failable = tilde_get_opt_ref(c, decl)))
	{
		value->kind = TBE_ADDRESS_OPTIONAL;
	}
}

void value_fold_optional(TildeContext *c, TBEValue *value)
{
	if (value->kind == TBE_ADDRESS_OPTIONAL)
	{
		TODO
		//tilde_emit_jump_to_optional_exit(c, tilde_load_natural_alignment(c, type_anyfault, value->optional, "optval"));
		value->kind = TBE_ADDRESS;
	}
}

void value_rvalue(TildeContext *c, TBEValue *value)
{
	if (value->kind == TBE_VALUE) return;
	value_fold_optional(c, value);
	value->reg = tilde_load_value(c, value);
	value->kind = TBE_VALUE;
}
