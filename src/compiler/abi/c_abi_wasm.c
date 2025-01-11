// Copyright (c) 2020 Christoffer Lerno. All rights reserved.
// Use of this source code is governed by a LGPLv3.0
// a copy of which can be found in the LICENSE file.

#include "compiler/c_abi_internal.h"

static ABIArgInfo *wasm_classify_argument_type(Type *type)
{
	type = type_lowering(type);
	if (type_is_abi_aggregate(type))
	{
		// Clang: Lower single-field structs to just pass a regular value. TODO: We
		// could do reasonable-size multiple-field structs too, using getExpand(),
		// though watch out for things like bitfields.
		Type *single_type = type_abi_find_single_struct_element(type);
		if (single_type) return abi_arg_new_direct_coerce_type(single_type);

		// For the experimental multivalue ABI, fully expand all other aggregates
		/*if (Kind == ABIKind::ExperimentalMV) {
			const RecordType *RT = Ty->getAs<RecordType>();
			ASSERT0(RT);
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
	return c_abi_classify_argument_type_default(type);
}

static ABIArgInfo *wasm_classify_return(Type *type)
{
	if (type_is_abi_aggregate(type))
	{
		Type *single_type = type_abi_find_single_struct_element(type);
		if (single_type) return abi_arg_new_direct_coerce_type(single_type);
		/*
		 * 			// For the experimental multivalue ABI, return all other aggregates
			if (Kind == ABIKind::ExperimentalMV)
				return ABIArgInfo::getDirect();
		 */
	}
	// Use default classification
	return c_abi_classify_return_type_default(type);
}

ABIArgInfo **wasm_create_params(Type **params)
{
	unsigned param_count = vec_size(params);
	if (!param_count) return NULL;
	ABIArgInfo **args = MALLOC(sizeof(ABIArgInfo) * param_count);
	for (unsigned i = 0; i < param_count; i++)
	{
		args[i] = wasm_classify_argument_type(type_lowering(params[i]));
	}
	return args;
}

void c_abi_func_create_wasm(FunctionPrototype *prototype)
{
	prototype->ret_abi_info = wasm_classify_return(type_lowering(prototype->abi_ret_type));
	if (prototype->ret_by_ref)
	{
		prototype->ret_by_ref_abi_info = wasm_classify_argument_type(type_get_ptr(prototype->ret_by_ref_type));
	}

	prototype->abi_args = wasm_create_params(prototype->param_types);
	prototype->abi_varargs = wasm_create_params(prototype->varargs);
}