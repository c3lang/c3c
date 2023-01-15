// Copyright (c) 2022 Christoffer Lerno. All rights reserved.
// Use of this source code is governed by a LGPLv3.0
// a copy of which can be found in the LICENSE file.

#include "tilde_internal.h"

TB_CallingConv calling_conv_from(CallABI abi)
{
	switch (abi)
	{
		case CALL_X86_STD:
			return TB_STDCALL;
		case CALL_C:
			return TB_CDECL;
		case CALL_X86_FAST:
		case CALL_X86_THIS:
		case CALL_X86_VECTOR:
		case CALL_X86_REG:
		case CALL_AAPCS:
		case CALL_AAPCS_VFP:
			REMINDER("Using C decl even though actual calling convention is different.");
			return TB_CDECL;
	}
}

TB_DebugType *tilde_get_debug_type(Type *type)
{
	return NULL;
}
TB_FunctionPrototype *tilde_get_func_prototype(TildeContext *c, FunctionPrototype *prototype)
{
	if (prototype->tb_prototype) return prototype->tb_prototype;
	TB_FunctionPrototype *proto =
			tb_prototype_create(c->module,
			                    calling_conv_from(prototype->call_abi),
								tildetype(prototype->abi_ret_type),
			                    tilde_get_debug_type(prototype->rtype),
			                    vec_size(prototype->param_types),
			                    prototype->variadic == VARIADIC_RAW);
	FOREACH_BEGIN(AbiType *param, prototype->abi_args)
		tb_prototype_add_param_named(proto, tildetype(param->type), "foek", NULL);
	FOREACH_END();
	prototype->tb_prototype = proto;
	return proto;
}

TB_DataType tildetype(Type *type)
{
	type = type_lowering(type);
	if (type->tb_set) return (TB_DataType) { .raw = type->tb_type };
	TB_DataType tb_type;
	switch (type->type_kind)
	{
		case TYPE_TYPEID:
		case TYPE_ANYERR:
			UNREACHABLE;
		case TYPE_FUNC:
			TODO
		case TYPE_VECTOR:
			tb_type = tildetype(type->array.base);
			tb_type.width = next_highest_power_of_2(type->array.len);
			break;
		case TYPE_F32:
			tb_type = TB_TYPE_F32;
			break;
		case TYPE_F64:
			tb_type = TB_TYPE_F64;
			break;
		case TYPE_VOID:
			tb_type = TB_TYPE_VOID;
			break;
		case TYPE_I8:
		case TYPE_U8:
			tb_type = TB_TYPE_I8;
			break;
		case TYPE_I16:
		case TYPE_U16:
			tb_type = TB_TYPE_I16;
			break;
		case TYPE_I32:
		case TYPE_U32:
			tb_type = TB_TYPE_I32;
			break;
		case TYPE_I64:
		case TYPE_U64:
			tb_type = TB_TYPE_I64;
			break;
		case TYPE_I128:
		case TYPE_U128:
			tb_type = (TB_DataType) { { TB_INT, 0, 128 } };
			break;
		case TYPE_POINTER:
			tb_type = TB_TYPE_PTR;
			break;
		case TYPE_BOOL:
			tb_type = TB_TYPE_BOOL;
			break;
		case TYPE_ANY:
		default:
			TODO
	}
	type->tb_set = 1;
	type->tb_type = tb_type.raw;
	return tb_type;
}
