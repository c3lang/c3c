// Copyright (c) 2020 Christoffer Lerno. All rights reserved.
// Use of this source code is governed by a LGPLv3.0
// a copy of which can be found in the LICENSE file.

#include "c_abi_internal.h"

static ABIArgInfo *wasm_classify_argument_type(Type *type)
{
	type = type_lowering(type);
	if (type_is_abi_aggregate(type))
	{
		// Ignore empty structs/unions.
		if (type_is_empty_record(type, true)) return abi_arg_ignore();
		// Clang: Lower single-field structs to just pass a regular value. TODO: We
		// could do reasonable-size multiple-field structs too, using getExpand(),
		// though watch out for things like bitfields.
		Type *single_type = type_abi_find_single_struct_element(type);
		if (single_type) return abi_arg_new_direct_coerce(abi_type_new_plain(single_type));

		// For the experimental multivalue ABI, fully expand all other aggregates
		/*if (Kind == ABIKind::ExperimentalMV) {
			const RecordType *RT = Ty->getAs<RecordType>();
			assert(RT);
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
		// Ignore empty
		if (type_is_empty_record(type, true)) return abi_arg_ignore();

		Type *single_type = type_abi_find_single_struct_element(type);
		if (single_type) return abi_arg_new_direct_coerce(abi_type_new_plain(single_type));
		/*
		 * 			// For the experimental multivalue ABI, return all other aggregates
			if (Kind == ABIKind::ExperimentalMV)
				return ABIArgInfo::getDirect();
		 */
	}
	// Use default classification
	return c_abi_classify_return_type_default(type);
}

void c_abi_func_create_wasm(FunctionSignature *signature)
{
	Type *rtype = abi_rtype(signature);
	Type *return_type = abi_returntype(signature);
	if (IS_FAILABLE(signature->rtype))
	{
		signature->failable_abi_info = wasm_classify_return(type_lowering(type_anyerr));
	}
	else
	{
		signature->ret_abi_info = wasm_classify_return(type_lowering(rtype));
	}

	// If we have a failable, then the return type is a parameter.
	if (IS_FAILABLE(signature->rtype) && rtype->type_kind != TYPE_VOID)
	{
		signature->ret_abi_info = wasm_classify_argument_type(type_get_ptr(type_lowering(rtype)));
	}

	Decl **params = signature->params;
	VECEACH(params, i)
	{
		params[i]->var.abi_info = wasm_classify_argument_type(type_lowering(params[i]->type));
	}
}