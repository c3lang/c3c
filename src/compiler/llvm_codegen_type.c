// Copyright (c) 2019 Christoffer Lerno. All rights reserved.
// Use of this source code is governed by the GNU LGPLv3.0 license
// a copy of which can be found in the LICENSE file.

#include "llvm_codegen_internal.h"

static inline LLVMTypeRef llvm_type_from_decl(GenContext *c, Decl *decl);
static inline LLVMTypeRef llvm_type_from_ptr(GenContext *context, Type *type);
static inline LLVMTypeRef llvm_type_from_array(GenContext *context, Type *type);
static void param_expand(GenContext *context, LLVMTypeRef** params_ref, Type *type);
static inline void add_func_type_param(GenContext *context, Type *param_type, ABIArgInfo *arg_info, LLVMTypeRef **params);

static inline LLVMTypeRef llvm_type_from_decl(GenContext *c, Decl *decl)
{
	switch (decl->decl_kind)
	{
		case DECL_VAR:
		case DECL_ENUM_CONSTANT:
		case DECL_FAULTVALUE:
		case DECL_POISONED:
		case DECL_BODYPARAM:
		case NON_TYPE_DECLS:
			UNREACHABLE
		case DECL_BITSTRUCT:
			return llvm_get_type(c, decl->bitstruct.base_type->type);
		case DECL_FUNC:
			UNREACHABLE
		case DECL_TYPEDEF:
			return llvm_get_type(c, decl->typedef_decl.type_info->type);
		case DECL_DISTINCT:
			return llvm_get_type(c, decl->distinct_decl.base_type);
		case DECL_STRUCT:
		{
			LLVMTypeRef *types = NULL;
			LLVMTypeRef type = LLVMStructCreateNamed(c->context, decl->name ? decl->name : ".anon");
			// Avoid recursive issues.
			decl->type->backend_type = type;
			Decl **members = decl->strukt.members;
			VECEACH(members, i)
			{
				Decl *member = members[i];
				if (member->padding)
				{
					vec_add(types, llvm_const_padding_type(c, member->padding));
				}
				vec_add(types, llvm_get_type(c, members[i]->type));
			}
			if (decl->strukt.padding)
			{
				vec_add(types, llvm_const_padding_type(c, decl->strukt.padding));
			}
			LLVMStructSetBody(type, types, vec_size(types), decl->is_packed);
			return type;
		}
		case DECL_UNION:
		{
			LLVMTypeRef type = LLVMStructCreateNamed(c->context, decl->name ? decl->name : ".anon");
			// Avoid recursive issues.
			decl->type->backend_type = type;
			Decl **members = decl->strukt.members;
			if (vec_size(members))
			{

				Decl *rep_type = members[decl->strukt.union_rep];
				LLVMTypeRef type_ref[2] = {
						llvm_get_type(c, rep_type->type),
						NULL
				};
				unsigned elements = 1;
				if (decl->strukt.padding)
				{
					type_ref[elements++] = llvm_const_padding_type(c, decl->strukt.padding);
				}
				LLVMStructSetBody(type, type_ref, elements, decl->is_packed);
			}
			else
			{
				LLVMStructSetBody(type, NULL, 0, true);
			}
			return type;
		}
		case DECL_ENUM:
			return llvm_get_type(c, decl->type);
		case DECL_FAULT:
			return llvm_get_type(c, type_iptr);
	}
	UNREACHABLE
}

static inline LLVMTypeRef llvm_type_from_ptr(GenContext *context, Type *type)
{
	if (type->canonical != type)
	{
		return type->backend_type = llvm_get_type(context, type->canonical);
	}
	if (type == type_voidptr)
	{
		return type->backend_type = llvm_get_ptr_type(context, type_char);
	}
	return type->backend_type = LLVMPointerType(llvm_get_type(context, type->pointer), /** TODO **/0);
}


static inline LLVMTypeRef llvm_type_from_array(GenContext *context, Type *type)
{
	if (type->canonical != type)
	{
		return type->backend_type = llvm_get_type(context, type->canonical);
	}

	return type->backend_type = LLVMArrayType(llvm_get_type(context, type->array.base), (unsigned)type->array.len);
}


static void param_expand(GenContext *context, LLVMTypeRef** params_ref, Type *type)
{
	switch (type->type_kind)
	{
		case TYPE_TYPEDEF:
			UNREACHABLE
		case TYPE_ARRAY:
			for (ArraySize i = type->array.len; i > 0; i--)
			{
				param_expand(context, params_ref, type->array.base);
			}
			return;
		case TYPE_STRUCT:
		{
			Decl **members = type->decl->strukt.members;
			VECEACH(members, i)
			{
				param_expand(context, params_ref, members[i]->type);
			}
			return;
		}
		case TYPE_ENUM:
		case TYPE_ANYERR:
		case TYPE_FAULTTYPE:
			param_expand(context, params_ref, type_lowering(type));
			return;
		case TYPE_UNION:
		{
			ByteSize largest = 0;
			Type *largest_type = NULL;
			Decl **members = type->decl->strukt.members;
			// Clang: Unions can be here only in degenerative cases - all the fields are same
			// after flattening. Thus we have to use the "largest" field.
			VECEACH(members, i)
			{
				if (type_size(type) > largest)
				{
					largest = type_size(type);
					type = type->canonical;
				}
			}
			if (!largest) return;
			param_expand(context, params_ref, largest_type);
			return;
		}
		default:
			// Type complex: return 2;
			vec_add(*params_ref, llvm_get_type(context, type));
			return;
	}

}

static inline void add_func_type_param(GenContext *context, Type *param_type, ABIArgInfo *arg_info, LLVMTypeRef **params)
{
	arg_info->param_index_start = (MemberIndex)vec_size(*params);
	switch (arg_info->kind)
	{
		case ABI_ARG_IGNORE:
			break;
		case ABI_ARG_INDIRECT:
			vec_add(*params, llvm_get_ptr_type(context, param_type));
			break;
		case ABI_ARG_EXPAND_COERCE:
			vec_add(*params, llvm_abi_type(context, arg_info->coerce_expand.lo));
			if (abi_type_is_valid(arg_info->coerce_expand.hi))
			{
				vec_add(*params, llvm_abi_type(context, arg_info->coerce_expand.hi));
			}
			break;
		case ABI_ARG_EXPAND:
			// Expanding a structs
			param_expand(context, params, param_type->canonical);
			// If we have padding, add it here.
			if (arg_info->expand.padding_type)
			{
				vec_add(*params, llvm_get_type(context, arg_info->expand.padding_type));
			}
			break;
		case ABI_ARG_DIRECT:
			vec_add(*params, llvm_get_type(context, param_type));
			break;
		case ABI_ARG_DIRECT_SPLIT_STRUCT:
		{
			// Normal direct.
			LLVMTypeRef coerce_type = llvm_get_type(context, arg_info->direct_struct_expand.type);
			for (unsigned idx = 0; idx < arg_info->direct_struct_expand.elements; idx++)
			{
				vec_add(*params, coerce_type);
			}
			break;
		}
		case ABI_ARG_DIRECT_COERCE_INT:
		{
			// Normal direct.
			LLVMTypeRef coerce_type = LLVMIntTypeInContext(context->context, type_size(param_type) * 8);
			vec_add(*params, coerce_type);
			break;
		}
		case ABI_ARG_DIRECT_COERCE:
		{
			// Normal direct.
			LLVMTypeRef coerce_type = llvm_get_type(context, arg_info->direct_coerce_type);
			vec_add(*params, coerce_type);
			break;
		}
		case ABI_ARG_DIRECT_PAIR:
			// Pairs are passed by param.
			vec_add(*params, llvm_abi_type(context, arg_info->direct_pair.lo));
			vec_add(*params, llvm_abi_type(context, arg_info->direct_pair.hi));
			break;
	}
	arg_info->param_index_end = (MemberIndex)vec_size(*params);
}

LLVMTypeRef llvm_update_prototype_abi(GenContext *context, FunctionPrototype *prototype, LLVMTypeRef **params)
{
	LLVMTypeRef retval = NULL;
	Type *call_return_type = prototype->abi_ret_type;
	ABIArgInfo *ret_arg_info = prototype->ret_abi_info;

	ret_arg_info->param_index_end = 0;
	ret_arg_info->param_index_start = 0;

	switch (ret_arg_info->kind)
	{
		case ABI_ARG_EXPAND:
			UNREACHABLE;
		case ABI_ARG_INDIRECT:
			vec_add(*params, llvm_get_ptr_type(context, call_return_type));
			retval = llvm_get_type(context, type_void);
			break;
		case ABI_ARG_EXPAND_COERCE:
		{
			LLVMTypeRef lo = llvm_abi_type(context, ret_arg_info->direct_pair.lo);
			if (!abi_type_is_valid(ret_arg_info->direct_pair.hi))
			{
				retval = lo;
				break;
			}
			LLVMTypeRef hi = llvm_abi_type(context, ret_arg_info->direct_pair.hi);
			retval = llvm_get_twostruct(context, lo, hi);
			break;
		}
		case ABI_ARG_IGNORE:
			retval = llvm_get_type(context, type_void);
			break;
		case ABI_ARG_DIRECT_PAIR:
		{
			LLVMTypeRef lo = llvm_abi_type(context, ret_arg_info->direct_pair.lo);
			LLVMTypeRef hi = llvm_abi_type(context, ret_arg_info->direct_pair.hi);
			retval = llvm_get_twostruct(context, lo, hi);
			break;
		}
		case ABI_ARG_DIRECT:
			retval = llvm_get_type(context, call_return_type);
			break;
		case ABI_ARG_DIRECT_SPLIT_STRUCT:
			UNREACHABLE
		case ABI_ARG_DIRECT_COERCE_INT:
			retval = LLVMIntTypeInContext(context->context, type_size(call_return_type) * 8);
			break;
		case ABI_ARG_DIRECT_COERCE:
			retval = llvm_get_type(context, ret_arg_info->direct_coerce_type);
			break;
	}

	// If it's failable and it's not void (meaning ret_abi_info will be NULL)
	if (prototype->ret_by_ref)
	{
		add_func_type_param(context, type_get_ptr(type_lowering(prototype->ret_by_ref_type)), prototype->ret_by_ref_abi_info, params);
	}

	// Add in all of the required arguments.
	VECEACH(prototype->param_types, i)
	{
		add_func_type_param(context, prototype->param_types[i], prototype->abi_args[i], params);
	}

	VECEACH(prototype->varargs, i)
	{
		add_func_type_param(context, prototype->varargs[i], prototype->abi_varargs[i], params);
	}
	return retval;
}

LLVMTypeRef llvm_func_type(GenContext *context, FunctionPrototype *prototype)
{
	LLVMTypeRef *params = NULL;
	LLVMTypeRef ret = llvm_update_prototype_abi(context, prototype, &params);
	return prototype->llvm_prototype = LLVMFunctionType(ret, params, vec_size(params), prototype->variadic == VARIADIC_RAW);
}


LLVMTypeRef llvm_get_pointee_type(GenContext *c, Type *any_type)
{
	any_type = any_type->canonical;
	assert(any_type->type_kind == TYPE_POINTER);
	if (any_type == type_voidptr) return llvm_get_type(c, type_char);
	return llvm_get_type(c, any_type->pointer);
}

LLVMTypeRef llvm_get_type(GenContext *c, Type *any_type)
{
	if (any_type->backend_type)
	{
		assert(LLVMGetTypeContext(any_type->backend_type) == c->context && "Should have been purged");
		return any_type->backend_type;
	}
	switch (any_type->type_kind)
	{
		case CT_TYPES:
			UNREACHABLE
		case TYPE_FAILABLE:
		case TYPE_FAILABLE_ANY:
			// If this is reachable, then we're not doing the proper lowering.
			UNREACHABLE
		case TYPE_TYPEID:
		case TYPE_ANYERR:
		case TYPE_FAULTTYPE:
			return any_type->backend_type = llvm_get_type(c, type_iptr->canonical);
		case TYPE_TYPEDEF:
			return any_type->backend_type = llvm_get_type(c, any_type->canonical);
		case TYPE_DISTINCT:
			return any_type->backend_type = llvm_get_type(c, any_type->decl->distinct_decl.base_type);
		case TYPE_ENUM:
			return any_type->backend_type = llvm_get_type(c, any_type->decl->enums.type_info->type->canonical);
		case TYPE_STRUCT:
		case TYPE_UNION:
		case TYPE_BITSTRUCT:
			return any_type->backend_type = llvm_type_from_decl(c, any_type->decl);
		case TYPE_FUNC:
			return any_type->backend_type = llvm_func_type(c, any_type->function.prototype);
		case TYPE_VOID:
			return any_type->backend_type = LLVMVoidTypeInContext(c->context);
		case TYPE_F64:
			return any_type->backend_type = LLVMDoubleTypeInContext(c->context);
		case TYPE_F16:
			return any_type->backend_type = LLVMHalfTypeInContext(c->context);
		case TYPE_F32:
			return any_type->backend_type = LLVMFloatTypeInContext(c->context);
		case TYPE_F128:
			return any_type->backend_type = LLVMFP128TypeInContext(c->context);
		case ALL_SIGNED_INTS:
		case ALL_UNSIGNED_INTS:
			return any_type->backend_type = LLVMIntTypeInContext(c->context, any_type->builtin.bitsize);
		case TYPE_BOOL:
			return any_type->backend_type = LLVMIntTypeInContext(c->context, 8U);
		case TYPE_POINTER:
			return any_type->backend_type = llvm_type_from_ptr(c, any_type);
		case TYPE_ARRAY:
		case TYPE_FLEXIBLE_ARRAY:
			return any_type->backend_type = llvm_type_from_array(c, any_type);
		case TYPE_SUBARRAY:
		{
			LLVMTypeRef base_type = llvm_get_type(c, type_get_ptr(any_type->array.base));
			LLVMTypeRef size_type = llvm_get_type(c, type_usize);
			LLVMTypeRef array_type = LLVMStructCreateNamed(c->context, any_type->name);
			LLVMTypeRef types[2] = { base_type, size_type };
			LLVMStructSetBody(array_type, types, 2, false);
			return any_type->backend_type = array_type;
		}
		case TYPE_ANY:
		{
			LLVMTypeRef pointer_type = llvm_get_type(c, type_voidptr);
			LLVMTypeRef type_type = llvm_get_type(c, type_typeid);
			LLVMTypeRef virtual_type = LLVMStructCreateNamed(c->context, any_type->name);
			LLVMTypeRef types[2] = { pointer_type, type_type };
			LLVMStructSetBody(virtual_type, types, 2, false);
			return any_type->backend_type = virtual_type;
		}
		case TYPE_VECTOR:
			return any_type->backend_type = LLVMVectorType(llvm_get_type(c, any_type->array.base), any_type->array.len);
	}
	UNREACHABLE;
}


LLVMTypeRef llvm_get_coerce_type(GenContext *c, ABIArgInfo *arg_info)
{
	switch (arg_info->kind)
	{
		case ABI_ARG_EXPAND_COERCE:
		{
			unsigned element_index = 0;
			LLVMTypeRef elements[4];
			// Add optional padding to make the data appear at the correct offset.
			if (arg_info->coerce_expand.offset_lo)
			{
				elements[element_index++] = llvm_const_padding_type(c, arg_info->coerce_expand.offset_lo);
			}
			elements[element_index++] = llvm_abi_type(c, arg_info->coerce_expand.lo);
			// Add optional padding to make the high field appear at the correct off.
			if (arg_info->coerce_expand.padding_hi)
			{
				elements[element_index++] = LLVMArrayType(llvm_get_type(c, type_char), arg_info->coerce_expand.padding_hi);
			}
			// Check if there is a top type as well.
			if (abi_type_is_valid(arg_info->coerce_expand.hi))
			{
				elements[element_index++] = llvm_abi_type(c, arg_info->coerce_expand.hi);
			}
			return LLVMStructTypeInContext(c->context, elements, element_index, arg_info->coerce_expand.packed);
		}
		case ABI_ARG_DIRECT_SPLIT_STRUCT:
		{
			LLVMTypeRef coerce_type = llvm_get_type(c, arg_info->direct_struct_expand.type);
			assert(arg_info->direct_struct_expand.elements > 1U);
			LLVMTypeRef *refs = MALLOC(sizeof(LLVMValueRef) * arg_info->direct_struct_expand.elements);
			for (unsigned i = 0; i < arg_info->direct_struct_expand.elements; i++)
			{
				refs[i] = coerce_type;
			}
			return LLVMStructTypeInContext(c->context, refs, arg_info->direct_struct_expand.elements, false);
		}
		case ABI_ARG_DIRECT_PAIR:
		{
			LLVMTypeRef lo = llvm_abi_type(c, arg_info->direct_pair.lo);
			LLVMTypeRef hi = llvm_abi_type(c, arg_info->direct_pair.hi);
			return llvm_get_twostruct(c, lo, hi);
		}
		case ABI_ARG_IGNORE:
		case ABI_ARG_DIRECT:
		case ABI_ARG_DIRECT_COERCE:
		case ABI_ARG_INDIRECT:
		case ABI_ARG_EXPAND:
		case ABI_ARG_DIRECT_COERCE_INT:
			UNREACHABLE
	}
	UNREACHABLE
}

LLVMTypeRef llvm_get_twostruct(GenContext *context, LLVMTypeRef lo, LLVMTypeRef hi)
{
	LLVMTypeRef types[2] = { lo, hi };
	return LLVMStructTypeInContext(context->context, types, 2, false);
}

LLVMTypeRef llvm_abi_type(GenContext *c, AbiType type)
{
	if (abi_type_is_type(type)) return llvm_get_type(c, type.type);
	return LLVMIntTypeInContext(c->context, type.int_bits_plus_1 - 1);
}


static inline LLVMValueRef llvm_generate_temp_introspection_global(GenContext *c, Type *type)
{
	assert(!type->backend_typeid);
	LLVMValueRef temp = LLVMAddGlobal(c->module, c->introspect_type, "tempid");
	type->backend_typeid = LLVMBuildPointerCast(c->builder, temp, llvm_get_type(c, type_typeid), "");
	return temp;
}

static inline LLVMValueRef llvm_generate_introspection_global(GenContext *c, LLVMValueRef original_global, Type *type, IntrospectType introspect_type,
                                                              Type *inner, size_t len, LLVMValueRef additional, bool is_external)
{
	if (original_global)
	{
		assert(type->backend_typeid);
	}
	LLVMValueRef values[INTROSPECT_INDEX_TOTAL] = {
			[INTROSPECT_INDEX_KIND] = LLVMConstInt(c->byte_type, introspect_type, false),
			[INTROSPECT_INDEX_SIZEOF] = LLVMConstInt(c->size_type, type_size(type), false),
			[INTROSPECT_INDEX_INNER] = inner ? llvm_get_typeid(c, inner) : llvm_get_zero(c, type_typeid),
			[INTROSPECT_INDEX_LEN] = LLVMConstInt(c->size_type,len, false),
			[INTROSPECT_INDEX_ADDITIONAL] = additional ? additional : LLVMConstArray(c->size_type, NULL, 0)
	};
	LLVMValueRef global_name;
	scratch_buffer_clear();
	scratch_buffer_append("ct$");
	type_mangle_introspect_name_to_buffer(type);
	if (additional)
	{
		LLVMValueRef constant = llvm_get_struct(c, values, INTROSPECT_INDEX_TOTAL);
		global_name = LLVMAddGlobal(c->module, LLVMTypeOf(constant), scratch_buffer_to_string());
		LLVMSetInitializer(global_name, constant);
	}
	else
	{
		LLVMValueRef strukt = llvm_get_struct_named(c->introspect_type, values, INTROSPECT_INDEX_TOTAL);
		global_name = LLVMAddGlobal(c->module, c->introspect_type, scratch_buffer_to_string());
		LLVMSetInitializer(global_name, strukt);
	}
	LLVMSetAlignment(global_name, llvm_abi_alignment(c, c->introspect_type));
	LLVMSetGlobalConstant(global_name, 1);
	if (is_external)
	{
		LLVMSetLinkage(global_name, LLVMExternalLinkage);
	}
	else
	{
		llvm_set_linkonce(c, global_name);
	}
	if (original_global)
	{
		LLVMReplaceAllUsesWith(original_global, global_name);
		LLVMDeleteGlobal(original_global);
	}
	else
	{
		type->backend_typeid = LLVMBuildPointerCast(c->builder, global_name, llvm_get_type(c, type_typeid), "");
	}
	return type->backend_typeid;
}
static LLVMValueRef llvm_get_introspection_for_builtin_type(GenContext *c, Type *type, IntrospectType introspect_type, int bits)
{
	return llvm_generate_introspection_global(c, NULL, type, introspect_type, NULL, 0, NULL, false);
}


static LLVMValueRef llvm_get_introspection_for_enum(GenContext *c, Type *type)
{
	Decl *decl = type->decl;
	bool is_external = decl->unit->module != c->code_module;
	bool is_dynamic = decl->is_dynamic;

	Decl **enum_vals = decl->enums.values;
	unsigned elements = vec_size(enum_vals);
	Decl **associated_values = decl->enums.parameters;
	if (is_external && is_dynamic)
	{
		elements = 0;
	}

	if (!is_dynamic) is_external = false;

	LLVMTypeRef subarray = llvm_get_type(c, type_chars);
	LLVMValueRef *values = elements ? malloc_arena(elements * sizeof(LLVMValueRef)) : NULL;

	bool obfuscate = decl->obfuscate;
	for (unsigned i = 0; i < elements; i++)
	{
		BEValue value;
		const char *name = enum_vals[i]->name;
		size_t len = strlen(name);
		scratch_buffer_clear();
		scratch_buffer_append(".enum.");
		scratch_buffer_append_unsigned_int(i);
		const char *name_desc = scratch_buffer_to_string();
		if (obfuscate)
		{
			len = strlen(name_desc);
			name = name_desc;
		}
		LLVMValueRef name_ref = llvm_emit_zstring_named(c, name, scratch_buffer_to_string());
		LLVMValueRef data[2] = { name_ref, llvm_const_int(c, type_usize, len) };
		values[i] = llvm_get_struct_named(subarray, data, 2);
	}
	LLVMValueRef names = llvm_get_array(subarray, values, elements);

	LLVMValueRef val = llvm_generate_introspection_global(c, NULL, type, INTROSPECT_TYPE_ENUM, type_flatten(type), elements, names, is_external);
	LLVMTypeRef val_type;


	VECEACH(associated_values, ai)
	{
		val_type = NULL;
		bool mixed = false;
		for (unsigned i = 0; i < elements; i++)
		{
			BEValue value;
			llvm_emit_expr(c, &value, enum_vals[i]->enum_constant.args[ai]);
			assert(!llvm_value_is_addr(&value));
			LLVMValueRef llvm_value = llvm_value_is_bool(&value) ? LLVMBuildZExt(c->builder, value.value, c->byte_type, "")
			                                                     : value.value;
			values[i] = llvm_value;
			if (!val_type)
			{
				val_type = LLVMTypeOf(llvm_value);
				continue;
			}
			if (val_type != LLVMTypeOf(llvm_value)) mixed = true;
		}
		Decl *associated_value = associated_values[ai];
		LLVMValueRef associated_value_arr = mixed ? llvm_get_packed_struct(c, values, elements)
				: llvm_get_array(val_type, values, elements);
		scratch_buffer_clear();
		scratch_buffer_append(decl->extname);
		scratch_buffer_append("$");
		scratch_buffer_append(associated_value->name);
		LLVMValueRef global_ref = llvm_add_global_raw(c,
		                                              scratch_buffer_to_string(),
		                                              LLVMTypeOf(associated_value_arr),
		                                              0);
		llvm_set_linkonce(c, global_ref);
		LLVMSetInitializer(global_ref, associated_value_arr);
		LLVMSetGlobalConstant(global_ref, true);
		if (mixed)
		{
			associated_value->backend_ref = llvm_emit_bitcast_ptr(c, global_ref, type_get_array(associated_value->type, elements));
		}
		else
		{
			associated_value->backend_ref = global_ref;
		}
	}
	return val;
}

static LLVMValueRef llvm_get_introspection_for_struct_union(GenContext *c, Type *type)
{
	Decl *decl = type->decl;
	Decl **decls = decl->strukt.members;
	LLVMValueRef ref = llvm_generate_temp_introspection_global(c, type);
	VECEACH(decls, i)
	{
		Decl *member_decl = decls[i];
		if (decl_is_struct_type(member_decl))
		{
			llvm_get_typeid(c, member_decl->type);
		}
	}

	return llvm_generate_introspection_global(c, ref, type, decl->decl_kind == DECL_UNION ? INTROSPECT_TYPE_UNION : INTROSPECT_TYPE_STRUCT,
											  NULL,
											  vec_size(decls), NULL, false);
}

static LLVMValueRef llvm_get_introspection_for_fault(GenContext *c, Type *type)
{
	Decl *decl = type->decl;
	Decl **fault_vals = decl->enums.values;
	unsigned elements = vec_size(fault_vals);
	LLVMValueRef ref = llvm_generate_temp_introspection_global(c, type);
	AlignSize store_align;
	for (unsigned i = 0; i < elements; i++)
	{
		scratch_buffer_clear();
		scratch_buffer_append(decl_get_extname(decl));
		scratch_buffer_append_char('$');
		Decl *val = fault_vals[i];
		scratch_buffer_append(val->name);
		LLVMValueRef global_name = LLVMAddGlobal(c->module, c->fault_type, scratch_buffer_to_string());
		LLVMSetAlignment(global_name, LLVMPreferredAlignmentOfGlobal(c->target_data, global_name));
		LLVMSetGlobalConstant(global_name, 1);

		LLVMValueRef vals[2] = { LLVMBuildPtrToInt(c->builder, ref, llvm_get_type(c, type_typeid), ""),
								 llvm_emit_aggregate_two(c, type_chars, llvm_emit_zstring_named(c, val->name, ".fault"),
		                                                      llvm_const_int(c, type_usize, strlen(val->name))) };

		LLVMSetInitializer(global_name, llvm_get_struct_named(c->fault_type, vals, 2));
		llvm_set_linkonce(c, global_name);
		val->backend_ref = LLVMBuildPointerCast(c->builder, global_name, llvm_get_type(c, type_typeid), "");
	}
	LLVMTypeRef element_type = llvm_get_type(c, type_typeid);
	LLVMValueRef* values = elements ? MALLOC(sizeof(LLVMValueRef) * elements) : NULL;
	for (unsigned i = 0; i < elements; i++)
	{
		values[i] = LLVMBuildBitCast(c->builder, fault_vals[i]->backend_ref, element_type, "");
	}
	return llvm_generate_introspection_global(c, ref, type, INTROSPECT_TYPE_FAULT, NULL, elements, NULL, false);
}


LLVMValueRef llvm_get_typeid(GenContext *c, Type *type)
{
	if (type->backend_typeid) return type->backend_typeid;

	switch (type->type_kind)
	{
		case TYPE_FAILABLE:
			return llvm_generate_introspection_global(c, NULL, type, INTROSPECT_TYPE_FAILABLE, type->failable, 0, NULL, false);
		case TYPE_FLEXIBLE_ARRAY:
			return llvm_generate_introspection_global(c, NULL, type, INTROSPECT_TYPE_ARRAY, type->array.base, 0, NULL, false);
		case TYPE_VECTOR:
			return llvm_generate_introspection_global(c, NULL, type, INTROSPECT_TYPE_VECTOR, type->array.base, type->array.len, NULL, false);
		case TYPE_ARRAY:
			return llvm_generate_introspection_global(c, NULL, type, INTROSPECT_TYPE_ARRAY, type->array.base, type->array.len, NULL, false);
		case TYPE_SUBARRAY:
			return llvm_generate_introspection_global(c, NULL, type, INTROSPECT_TYPE_SUBARRAY, type->array.base, 0, NULL, false);
		case TYPE_POINTER:
			return llvm_generate_introspection_global(c, NULL, type, INTROSPECT_TYPE_POINTER, type->pointer, 0, NULL, false);
		case TYPE_DISTINCT:
			return llvm_generate_introspection_global(c, NULL, type, INTROSPECT_TYPE_DISTINCT, type->decl->distinct_decl.base_type, 0, NULL, false);
		case TYPE_ENUM:
			return llvm_get_introspection_for_enum(c, type);
		case TYPE_FAULTTYPE:
			return llvm_get_introspection_for_fault(c, type);
		case TYPE_STRUCT:
		case TYPE_UNION:
			return llvm_get_introspection_for_struct_union(c, type);
		case TYPE_FUNC:
			if (type->function.prototype->raw_type == type)
			{
				LLVMValueRef ref = llvm_generate_temp_introspection_global(c, type);
				return llvm_generate_introspection_global(c, ref, type, INTROSPECT_TYPE_FUNC, NULL, 0, NULL, false);
			}
			return llvm_get_typeid(c, type->function.prototype->raw_type);
		case TYPE_BITSTRUCT:
		{
			LLVMValueRef ref = llvm_generate_temp_introspection_global(c, type);
			return llvm_generate_introspection_global(c, ref, type, INTROSPECT_TYPE_BITSTRUCT, NULL, 0, NULL, false);
		}
		case TYPE_TYPEDEF:
			return llvm_get_typeid(c, type->canonical);
		case TYPE_INFERRED_ARRAY:
		case TYPE_UNTYPED_LIST:
		case TYPE_FAILABLE_ANY:
		case TYPE_TYPEINFO:
			UNREACHABLE
		case TYPE_VOID:
			return llvm_get_introspection_for_builtin_type(c, type, INTROSPECT_TYPE_VOID, 0);
		case TYPE_BOOL:
			return llvm_get_introspection_for_builtin_type(c, type, INTROSPECT_TYPE_BOOL, 0);
		case ALL_SIGNED_INTS:
			return llvm_get_introspection_for_builtin_type(c, type, INTROSPECT_TYPE_SIGNED_INT,
			                                               type_kind_bitsize(type->type_kind));
		case ALL_UNSIGNED_INTS:
			return llvm_get_introspection_for_builtin_type(c,
			                                               type,
			                                               INTROSPECT_TYPE_UNSIGNED_INT,
			                                               type_kind_bitsize(type->type_kind));
		case ALL_FLOATS:
			return llvm_get_introspection_for_builtin_type(c,
			                                               type,
			                                               INTROSPECT_TYPE_FLOAT,
			                                               type_kind_bitsize(type->type_kind));
		case TYPE_ANYERR:
			return llvm_get_introspection_for_builtin_type(c, type, INTROSPECT_TYPE_ANYERR, 0);
		case TYPE_ANY:
			return llvm_get_introspection_for_builtin_type(c, type, INTROSPECT_TYPE_ANY, 0);
		case TYPE_TYPEID:
			return llvm_get_introspection_for_builtin_type(c, type, INTROSPECT_TYPE_TYPEID, 0);
		case TYPE_POISONED:
			UNREACHABLE
	}
	UNREACHABLE
}