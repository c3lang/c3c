// Copyright (c) 2019 Christoffer Lerno. All rights reserved.
// Use of this source code is governed by the GNU LGPLv3.0 license
// a copy of which can be found in the LICENSE file.

#include "llvm_codegen_internal.h"

static inline LLVMTypeRef llvm_type_from_decl(GenContext *c, Decl *decl);

static inline LLVMTypeRef llvm_type_from_array(GenContext *context, Type *type);
static void param_expand(GenContext *context, LLVMTypeRef** params_ref, Type *type);
static inline void add_func_type_param(GenContext *c, Type *param_type, ABIArgInfo *arg_info, LLVMTypeRef **params);

static inline LLVMTypeRef llvm_type_from_decl(GenContext *c, Decl *decl)
{
	switch (decl->decl_kind)
	{
		case DECL_ERASED:
		case NON_TYPE_DECLS:
		case DECL_FNTYPE:
		case DECL_INTERFACE:
			UNREACHABLE
		case DECL_BITSTRUCT:
			return llvm_get_type(c, decl->strukt.container_type->type);
		case DECL_FUNC:
			UNREACHABLE
		case DECL_TYPEDEF:
			return llvm_get_type(c, decl->type);
		case DECL_DISTINCT:
			return llvm_get_type(c, decl->distinct->type);
		case DECL_STRUCT:
		{
			LLVMTypeRef *types = NULL;
			LLVMTypeRef type = LLVMStructCreateNamed(c->context, decl->name ? decl->name : ".anon");
			// Avoid recursive issues.
			decl->type->backend_type = type;
			Decl **members = decl->strukt.members;
			FOREACH(Decl *, member, members)
			{
				if (member->padding)
				{
					vec_add(types, llvm_const_padding_type(c, member->padding));
				}
				vec_add(types, llvm_get_type(c, member->type));
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
			FOREACH(Decl *, member, type->decl->strukt.members)
			{
				param_expand(context, params_ref, member->type);
			}
			return;
		}
		case TYPE_ENUM:
		case TYPE_ANYFAULT:
		case TYPE_FAULTTYPE:
			param_expand(context, params_ref, type_lowering(type));
			return;
		case TYPE_UNION:
		{
			ByteSize largest = 0;
			Type *largest_type = NULL;
			// Clang: Unions can be here only in degenerative cases - all the fields are same
			// after flattening. Thus we have to use the "largest" field.
			FOREACH(Decl *, member, type->decl->strukt.members)
			{
				Type *member_type = member->type;
				if (type_size(member_type) > largest)
				{
					largest = type_size(member_type);
					largest_type = type_flatten(member_type);
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

static inline void add_func_type_param(GenContext *c, Type *param_type, ABIArgInfo *arg_info, LLVMTypeRef **params)
{
	arg_info->param_index_start = (ArrayIndex)vec_size(*params);
	switch (arg_info->kind)
	{
		case ABI_ARG_IGNORE:
			break;
		case ABI_ARG_INDIRECT:
			vec_add(*params, c->ptr_type);
			break;
		case ABI_ARG_EXPAND_COERCE:
			vec_add(*params, llvm_get_type(c, arg_info->coerce_expand.lo));
			vec_add(*params, llvm_get_type(c, arg_info->coerce_expand.hi));
			break;
		case ABI_ARG_EXPAND:
			// Expanding a structs
			param_expand(c, params, param_type->canonical);
			break;
		case ABI_ARG_DIRECT:
			vec_add(*params, llvm_get_type(c, param_type));
			break;
		case ABI_ARG_DIRECT_SPLIT_STRUCT_I32:
		{
			// Normal direct.
			LLVMTypeRef coerce_type = llvm_get_type(c, type_uint);
			for (unsigned idx = 0; idx < arg_info->direct_struct_expand; idx++)
			{
				vec_add(*params, coerce_type);
			}
			break;
		}
		case ABI_ARG_DIRECT_COERCE_INT:
		{
			// Normal direct.
			vec_add(*params, LLVMIntTypeInContext(c->context, type_size(param_type) * 8));
			break;
		}
		case ABI_ARG_DIRECT_COERCE:
		{
			// Normal direct.
			vec_add(*params, llvm_get_type(c, arg_info->direct_coerce_type));
			break;
		}
		case ABI_ARG_DIRECT_PAIR:
			// Pairs are passed by param.
			vec_add(*params, llvm_abi_type(c, arg_info->direct_pair.lo));
			vec_add(*params, llvm_abi_type(c, arg_info->direct_pair.hi));
			break;
	}
	arg_info->param_index_end = (ArrayIndex)vec_size(*params);
}

LLVMTypeRef llvm_update_prototype_abi(GenContext *c, FunctionPrototype *prototype, LLVMTypeRef **params)
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
			vec_add(*params, c->ptr_type);
			retval = llvm_get_type(c, type_void);
			break;
		case ABI_ARG_EXPAND_COERCE:
		{
			LLVMTypeRef lo = llvm_get_type(c, ret_arg_info->coerce_expand.lo);
			LLVMTypeRef hi = llvm_get_type(c, ret_arg_info->coerce_expand.hi);
			retval = llvm_get_twostruct(c, lo, hi);
			break;
		}
		case ABI_ARG_IGNORE:
			retval = llvm_get_type(c, type_void);
			break;
		case ABI_ARG_DIRECT_PAIR:
		{
			LLVMTypeRef lo = llvm_abi_type(c, ret_arg_info->direct_pair.lo);
			LLVMTypeRef hi = llvm_abi_type(c, ret_arg_info->direct_pair.hi);
			retval = llvm_get_twostruct(c, lo, hi);
			break;
		}
		case ABI_ARG_DIRECT:
			retval = llvm_get_type(c, call_return_type);
			break;
		case ABI_ARG_DIRECT_SPLIT_STRUCT_I32:
			UNREACHABLE
		case ABI_ARG_DIRECT_COERCE_INT:
			retval = LLVMIntTypeInContext(c->context, type_size(call_return_type) * 8);
			break;
		case ABI_ARG_DIRECT_COERCE:
			retval = llvm_get_type(c, ret_arg_info->direct_coerce_type);
			break;
	}

	// If it's optional and it's not void (meaning ret_abi_info will be NULL)
	if (prototype->ret_by_ref)
	{
		add_func_type_param(c, type_get_ptr(type_lowering(prototype->ret_by_ref_type)), prototype->ret_by_ref_abi_info, params);
	}

	// Add in all of the required arguments.
	FOREACH_IDX(i, Type *, type, prototype->param_types)
	{
		add_func_type_param(c, type, prototype->abi_args[i], params);
	}

	FOREACH_IDX(j, Type *, type, prototype->varargs)
	{
		add_func_type_param(c, type, prototype->abi_varargs[j], params);
	}
	return retval;
}

LLVMTypeRef llvm_func_type(GenContext *context, FunctionPrototype *prototype)
{
	LLVMTypeRef *params = NULL;
	LLVMTypeRef ret = llvm_update_prototype_abi(context, prototype, &params);
	return LLVMFunctionType(ret, params, vec_size(params), prototype->raw_variadic);
}


LLVMTypeRef llvm_get_pointee_type(GenContext *c, Type *any_type)
{
	any_type = type_lowering(any_type);
	ASSERT0(any_type->type_kind == TYPE_POINTER);
	if (any_type == type_voidptr) return llvm_get_type(c, type_char);
	return llvm_get_type(c, any_type->pointer);
}

bool llvm_types_are_similar(LLVMTypeRef original, LLVMTypeRef coerce)
{
	if (original == coerce) return true;
	if (LLVMGetTypeKind(original) != LLVMStructTypeKind) return false;
	if (LLVMGetTypeKind(coerce) != LLVMStructTypeKind) return false;
	unsigned types = LLVMCountStructElementTypes(original);
	if (types != LLVMCountStructElementTypes(coerce)) return false;
	for (unsigned i = 0; i < types; i++)
	{
		if (LLVMStructGetTypeAtIndex(original, i) != LLVMStructGetTypeAtIndex(coerce, i)) return false;
	}
	return true;
}

LLVMTypeRef llvm_get_type(GenContext *c, Type *any_type)
{
	if (any_type->backend_type)
	{
		ASSERT0(LLVMGetTypeContext(any_type->backend_type) == c->context && "Should have been purged");
		return any_type->backend_type;
	}
	Type *type = type_lowering(any_type);
	if (type != any_type)
	{
		return any_type->backend_type = llvm_get_type(c, type);
	}
	switch (any_type->type_kind)
	{
		case LOWERED_TYPES:
			// If this is reachable, then we're not doing the proper lowering.
			UNREACHABLE
		case TYPE_STRUCT:
		case TYPE_UNION:
			return any_type->backend_type = llvm_type_from_decl(c, any_type->decl);
		case TYPE_FUNC_RAW:
			return any_type->backend_type = llvm_func_type(c, type_get_resolved_prototype(any_type));
		case TYPE_VOID:
			return any_type->backend_type = LLVMVoidTypeInContext(c->context);
		case TYPE_F64:
			return any_type->backend_type = LLVMDoubleTypeInContext(c->context);
		case TYPE_F16:
			return any_type->backend_type = LLVMHalfTypeInContext(c->context);
		case TYPE_BF16:
			return any_type->backend_type = LLVMBFloatTypeInContext(c->context);
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
		case TYPE_FUNC_PTR:
			ASSERT0(c->ptr_type);
			return any_type->backend_type = c->ptr_type;
		case TYPE_ARRAY:
		case TYPE_FLEXIBLE_ARRAY:
			return any_type->backend_type = llvm_type_from_array(c, any_type);
		case TYPE_SLICE:
		{
			LLVMTypeRef array_type = LLVMStructCreateNamed(c->context, any_type->name);
			LLVMTypeRef types[2] = { c->ptr_type, c->size_type };
			LLVMStructSetBody(array_type, types, 2, false);
			return any_type->backend_type = array_type;
		}
		case TYPE_ANY:
		{
			LLVMTypeRef virtual_type = LLVMStructCreateNamed(c->context, any_type->name);
			LLVMTypeRef types[2] = { c->ptr_type, c->typeid_type };
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
		case ABI_ARG_DIRECT_SPLIT_STRUCT_I32:
		{
			LLVMTypeRef coerce_type = llvm_get_type(c, type_uint);
			ASSERT0(arg_info->direct_struct_expand > 1U && arg_info->direct_struct_expand < 10);
			LLVMTypeRef refs[10];
			for (unsigned i = 0; i < arg_info->direct_struct_expand; i++)
			{
				refs[i] = coerce_type;
			}
			return LLVMStructTypeInContext(c->context, refs, arg_info->direct_struct_expand, false);
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
		case ABI_ARG_EXPAND_COERCE:
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
	ASSERT0(!type->backend_typeid);
	LLVMValueRef temp = LLVMAddGlobal(c->module, c->introspect_type, "tempid");
	type->backend_typeid = LLVMBuildPtrToInt(c->builder, temp, c->typeid_type, "");
	return temp;
}

static inline LLVMValueRef llvm_generate_introspection_global(GenContext *c, LLVMValueRef original_global, Type *type, IntrospectType introspect_type,
															  Type *inner, size_t len, LLVMValueRef additional, bool is_external)
{
	// Push the builder
	void *builder = c->builder;
	c->builder = c->global_builder;

	if (original_global)
	{
		ASSERT0(type->backend_typeid);
	}
	ASSERT0(type == type->canonical);
	Type *parent_type = type_find_parent_type(type);
	LLVMValueRef parent_typeid;
	LLVMValueRef global_name = NULL;
	if (!additional)
	{
		scratch_buffer_clear();
		scratch_buffer_append("$ct.");
		type_mangle_introspect_name_to_buffer(type);
		global_name = LLVMAddGlobal(c->module, c->introspect_type, scratch_buffer_to_string());
		type->backend_typeid = LLVMBuildPtrToInt(c->builder, global_name, c->typeid_type, "");
	}
	LLVMValueRef values[INTROSPECT_INDEX_TOTAL] = {
			[INTROSPECT_INDEX_KIND] = LLVMConstInt(c->byte_type, introspect_type, false),
			[INTROSPECT_INDEX_PARENTOF] = parent_type ? llvm_get_typeid(c, parent_type->canonical) : LLVMConstNull(c->typeid_type),
			[INTROSPECT_INDEX_DTABLE] = LLVMConstNull(c->ptr_type),
			[INTROSPECT_INDEX_SIZEOF] = LLVMConstInt(c->size_type, type->type_kind == TYPE_FUNC_RAW ? type_size(type_voidptr) : type_size(type), false),
			[INTROSPECT_INDEX_INNER] = inner ? llvm_get_typeid(c, inner) : llvm_get_zero(c, type_typeid),
			[INTROSPECT_INDEX_LEN] = LLVMConstInt(c->size_type,len, false),
			[INTROSPECT_INDEX_ADDITIONAL] = additional ? additional : LLVMConstArray(c->size_type, NULL, 0)
	};
	if (additional)
	{
		scratch_buffer_clear();
		scratch_buffer_append("$ct.");
		type_mangle_introspect_name_to_buffer(type);
		LLVMValueRef constant = llvm_get_struct(c, values, INTROSPECT_INDEX_TOTAL);
		global_name = LLVMAddGlobal(c->module, LLVMTypeOf(constant), scratch_buffer_to_string());
		LLVMSetInitializer(global_name, constant);
	}
	else
	{
		LLVMValueRef strukt = llvm_get_struct_named(c->introspect_type, values, INTROSPECT_INDEX_TOTAL);
		LLVMSetInitializer(global_name, strukt);
	}
	LLVMSetAlignment(global_name, llvm_abi_alignment(c, c->introspect_type));
	LLVMSetGlobalConstant(global_name, 0);
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
		type->backend_typeid = LLVMBuildPtrToInt(c->builder, global_name, c->typeid_type, "");
	}
	c->builder = builder;
	return type->backend_typeid;
}
static LLVMValueRef llvm_get_introspection_for_builtin_type(GenContext *c, Type *type, IntrospectType introspect_type, int bits)
{
	return llvm_generate_introspection_global(c, NULL, type, introspect_type, NULL, 0, NULL, false);
}


static LLVMValueRef llvm_get_introspection_for_enum(GenContext *c, Type *type)
{

	void *builder = c->builder;
	c->builder = c->global_builder;

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

	LLVMValueRef *values = elements ? malloc_arena(elements * sizeof(LLVMValueRef)) : NULL;

	bool obfuscate = decl->obfuscate;
	for (unsigned i = 0; i < elements; i++)
	{
		const char *name = enum_vals[i]->name;
		scratch_buffer_clear();
		scratch_buffer_append(".enum.");
		if (!obfuscate)
		{
			scratch_buffer_append(name);
		}
		else
		{
			scratch_buffer_append_unsigned_int(i);
		}
		const char *name_desc = scratch_buffer_to_string();
		if (obfuscate)
		{
			name = name_desc;
		}
		values[i] = llvm_emit_string_const(c, name, scratch_buffer_to_string());
	}
	LLVMValueRef names = llvm_get_array(c->chars_type, values, elements);

	LLVMValueRef val = llvm_generate_introspection_global(c, NULL, type, INTROSPECT_TYPE_ENUM, type_base(type), elements, names, is_external);
	LLVMTypeRef val_type;

	unsigned count = vec_size(associated_values);
	for (unsigned ai = 0; ai < count; ai++)
	{
		val_type = NULL;
		bool mixed = false;
		for (unsigned i = 0; i < elements; i++)
		{
			BEValue value;
			llvm_emit_expr_global_value(c, &value, enum_vals[i]->enum_constant.args[ai]);
			LLVMValueRef llvm_value = llvm_load_value_store(c, &value);
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
		scratch_buffer_set_extern_decl_name(decl, true);
		scratch_buffer_append("$");
		scratch_buffer_append(associated_value->name);
		LLVMValueRef global_ref = llvm_add_global_raw(c,
													  scratch_buffer_to_string(),
													  LLVMTypeOf(associated_value_arr),
													  0);
		llvm_set_linkonce(c, global_ref);
		LLVMSetInitializer(global_ref, associated_value_arr);
		LLVMSetGlobalConstant(global_ref, true);
		associated_value->backend_ref = global_ref;
	}
	c->builder = builder;
	return val;
}

static LLVMValueRef llvm_get_introspection_for_struct_union(GenContext *c, Type *type)
{
	void *builder = c->builder;
	c->builder = c->global_builder;

	Decl *decl = type->decl;
	Decl **decls = decl->strukt.members;
	LLVMValueRef ref = llvm_generate_temp_introspection_global(c, type);
	FOREACH(Decl *, member_decl, decls)
	{
		if (decl_is_struct_type(member_decl))
		{
			llvm_get_typeid(c, member_decl->type);
		}
	}

	c->builder = builder;
	return llvm_generate_introspection_global(c, ref, type, decl->decl_kind == DECL_UNION ? INTROSPECT_TYPE_UNION : INTROSPECT_TYPE_STRUCT,
											  NULL,
											  vec_size(decls), NULL, false);
}

static LLVMValueRef llvm_get_introspection_for_fault(GenContext *c, Type *type)
{
	void *builder = c->builder;
	c->builder = c->global_builder;

	Decl *decl = type->decl;
	Decl **fault_vals = decl->enums.values;
	unsigned elements = vec_size(fault_vals);
	LLVMValueRef ref = llvm_generate_temp_introspection_global(c, type);
	for (unsigned i = 0; i < elements; i++)
	{
		scratch_buffer_set_extern_decl_name(decl, true);
		scratch_buffer_append_char('$');
		Decl *val = fault_vals[i];
		scratch_buffer_append(val->name);
		LLVMValueRef global_name = LLVMAddGlobal(c->module, c->fault_type, scratch_buffer_to_string());
		LLVMSetAlignment(global_name, LLVMPreferredAlignmentOfGlobal(c->target_data, global_name));
		LLVMSetGlobalConstant(global_name, 1);

		LLVMValueRef vals[3] = { LLVMBuildPtrToInt(c->builder, ref, c->typeid_type, ""),
								 llvm_emit_string_const(c, val->name, ".fault"),
								 llvm_const_int(c, type_usz, val->enum_constant.ordinal + 1 )};

		LLVMSetInitializer(global_name, llvm_get_struct_named(c->fault_type, vals, 3));
		llvm_set_linkonce(c, global_name);
		val->backend_ref = LLVMBuildPtrToInt(c->builder, global_name, c->typeid_type, "");
	}
	LLVMValueRef* values = elements ? MALLOC(sizeof(LLVMValueRef) * elements) : NULL;
	for (unsigned i = 0; i < elements; i++)
	{
		values[i] = fault_vals[i]->backend_ref;
	}
	c->builder = builder;
	return llvm_generate_introspection_global(c, ref, type, INTROSPECT_TYPE_FAULT, NULL, elements, NULL, false);
}


LLVMValueRef llvm_get_typeid(GenContext *c, Type *type)
{
	if (type->backend_typeid) return type->backend_typeid;

	switch (type->type_kind)
	{
		case TYPE_OPTIONAL:
			return llvm_generate_introspection_global(c, NULL, type, INTROSPECT_TYPE_OPTIONAL, type->optional, 0, NULL, false);
		case TYPE_FLEXIBLE_ARRAY:
			return llvm_generate_introspection_global(c, NULL, type, INTROSPECT_TYPE_ARRAY, type->array.base, 0, NULL, false);
		case TYPE_VECTOR:
			return llvm_generate_introspection_global(c, NULL, type, INTROSPECT_TYPE_VECTOR, type->array.base, type->array.len, NULL, false);
		case TYPE_ARRAY:
			return llvm_generate_introspection_global(c, NULL, type, INTROSPECT_TYPE_ARRAY, type->array.base, type->array.len, NULL, false);
		case TYPE_SLICE:
			return llvm_generate_introspection_global(c, NULL, type, INTROSPECT_TYPE_SLICE, type->array.base, 0, NULL, false);
		case TYPE_ANY:
			return llvm_generate_introspection_global(c, NULL, type, INTROSPECT_TYPE_ANY, NULL, 0, NULL, false);
		case TYPE_INTERFACE:
			return llvm_generate_introspection_global(c, NULL, type, INTROSPECT_TYPE_INTERFACE, NULL, 0, NULL, false);
		case TYPE_POINTER:
			return llvm_generate_introspection_global(c, NULL, type, INTROSPECT_TYPE_POINTER, type->pointer, 0, NULL, false);
		case TYPE_DISTINCT:
			return llvm_generate_introspection_global(c, NULL, type, INTROSPECT_TYPE_DISTINCT, type->decl->distinct->type, 0, NULL, false);
		case TYPE_ENUM:
			return llvm_get_introspection_for_enum(c, type);
		case TYPE_FAULTTYPE:
			return llvm_get_introspection_for_fault(c, type);
		case TYPE_STRUCT:
		case TYPE_UNION:
			return llvm_get_introspection_for_struct_union(c, type);
		case TYPE_FUNC_PTR:
			type = type->pointer;
			FALLTHROUGH;
		case TYPE_FUNC_RAW:
			if (type->function.prototype->raw_type == type)
			{
				LLVMValueRef ref = llvm_generate_temp_introspection_global(c, type);
				return llvm_generate_introspection_global(c, ref, type, INTROSPECT_TYPE_FUNC, NULL, 0, NULL, false);
			}
			return llvm_get_typeid(c, type->function.prototype->raw_type);
		case TYPE_BITSTRUCT:
		{
			LLVMValueRef ref = llvm_generate_temp_introspection_global(c, type);
			return llvm_generate_introspection_global(c, ref, type, INTROSPECT_TYPE_BITSTRUCT, type->decl->strukt.container_type->type, 0, NULL, false);
		}
		case TYPE_TYPEDEF:
			return llvm_get_typeid(c, type->canonical);
		case CT_TYPES:
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
		case TYPE_ANYFAULT:
			return llvm_get_introspection_for_builtin_type(c, type, INTROSPECT_TYPE_ANYFAULT, 0);
		case TYPE_TYPEID:
			return llvm_get_introspection_for_builtin_type(c, type, INTROSPECT_TYPE_TYPEID, 0);
	}
	UNREACHABLE
}