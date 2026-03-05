// Copyright (c) 2020 Christoffer Lerno. All rights reserved.
// Use of this source code is governed by a LGPLv3.0
// a copy of which can be found in the LICENSE file.

#include "compiler/c_abi_internal.h"

static ABIArgInfo *wasm_classify_argument_type(ParamInfo param)
{
	Type *type = type_lowering(param.type);
	if (type_is_abi_aggregate(type))
	{
		// Clang: Lower single-field structs to just pass a regular value. TODO: We
		// could do reasonable-size multiple-field structs too, using getExpand(),
		// though watch out for things like bitfields.
		Type *single_type = type_abi_find_single_struct_element(type, true);
		if (single_type) return abi_arg_new_direct_coerce_type(abi_type_get(single_type), param);

		// For the experimental multivalue ABI, fully expand all other aggregates
		/*if (Kind == ABIKind::ExperimentalMV) {
			const RecordType *RT = Ty->getAs<RecordType>();
			ASSERT(RT);
			bool HasBitField = false;
			for (auto *Field : RT->getDecl()->fields()) {
				if (Field->isBitField()) {
					HasBitField = true;
					break;
				}
			}
			if (!HasBitField)
				return ABIArgInfo::getExpand();
		}*/
	}

	// Otherwise just do the default thing.
	return c_abi_classify_argument_type_default(param);
}

static ABIArgInfo *wasm_classify_return(ParamInfo param)
{
	Type *type = type_lowering(param.type);
	if (type_is_abi_aggregate(type))
	{
		Type *single_type = type_abi_find_single_struct_element(type, true);
		if (single_type) return abi_arg_new_direct_coerce_type(abi_type_get(single_type), param);
		/*
		 * 			// For the experimental multivalue ABI, return all other aggregates
			if (Kind == ABIKind::ExperimentalMV)
				return ABIArgInfo::getDirect();
		 */
	}
	// Use default classification
	return c_abi_classify_return_type_default(param);
}

ABIArgInfo **wasm_create_params(ParamInfo *params, unsigned param_count)
{
	if (!param_count) return NULL;
	ABIArgInfo **args = MALLOC(sizeof(ABIArgInfo) * param_count);
	for (unsigned i = 0; i < param_count; i++)
	{
		args[i] = wasm_classify_argument_type(params[i]);
	}
	return args;
}

void c_abi_func_create_wasm(FunctionPrototype *prototype, ParamInfo *params, unsigned param_count, ParamInfo *vaargs, unsigned vaarg_count)
{
	prototype->ret_abi_info = wasm_classify_return(prototype->return_info);
	prototype->abi_args = wasm_create_params(params, param_count);
	prototype->abi_varargs = wasm_create_params(vaargs, vaarg_count);
}