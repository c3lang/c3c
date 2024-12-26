// Copyright (c) 2019 Christoffer Lerno. All rights reserved.
// Use of this source code is governed by the GNU LGPLv3.0 license
// a copy of which can be found in the LICENSE file.


#include "llvm_codegen_internal.h"

static void llvm_append_xxlizer(GenContext *c, unsigned  priority, bool is_initializer, LLVMValueRef function);
static inline void llvm_emit_return_value(GenContext *context, LLVMValueRef value);
static void llvm_expand_from_args(GenContext *c, Type *type, LLVMValueRef ref, unsigned *index, AlignSize alignment);
static inline void llvm_process_parameter_value(GenContext *c, Decl *decl, ABIArgInfo *info, unsigned *index);
static inline void llvm_emit_func_parameter(GenContext *context, Decl *decl, ABIArgInfo *abi_info, unsigned *index, unsigned real_index);
static inline void llvm_emit_body(GenContext *c, LLVMValueRef function, FunctionPrototype *prototype, Signature *signature, Ast *body, Decl *decl);


/**
 * Erases the current block if it is empty.
 * @param c the context to use.
 * @return true if the block was erased.
 */
bool llvm_emit_check_block_branch(GenContext *c)
{
	if (!c->current_block) return false;
	// If it's not used, we can delete the previous block and skip the branch.
	// Unless it is the entry block or a label target for jumps
	// These empty blocks will occur when doing branches.
	// Consider:
	// while (1)
	// {
	//   break;
	//   break;
	// }
	// Naively we'd output
	// br label %for.cond  - 1st break
	// br label %for.cond  - 2nd break
	// br label %for.cond  - end of scope
	//
	// The fix is to introduce a new block after a break:
	// br label %for.cond
	// jmp:
	// br label %for.cond
	// jmp.1:
	// br label %for.cond
	//
	// But this leaves us with blocks that have no parent.
	// Consequently, we will delete those and realize that
	// we then have no need for emitting a br.
	if (c->current_block != c->first_block
		&& !LLVMGetFirstUse(LLVMBasicBlockAsValue(c->current_block)))
	{
		LLVMDeleteBasicBlock(c->current_block);
		c->current_block = NULL;
		return false;
	}
	return true;
};

void llvm_emit_br(GenContext *c, LLVMBasicBlockRef next_block)
{
	if (!llvm_emit_check_block_branch(c)) return;
	c->current_block = NULL;
	LLVMBuildBr(c->builder, next_block);
}


void llvm_emit_block(GenContext *c, LLVMBasicBlockRef next_block)
{
	ASSERT0(c->current_block == NULL);
	LLVMAppendExistingBasicBlock(c->cur_func.ref, next_block);
	LLVMPositionBuilderAtEnd(c->builder, next_block);
	c->current_block = next_block;
}

static void llvm_expand_from_args(GenContext *c, Type *type, LLVMValueRef ref, unsigned *index, AlignSize alignment)
{
	switch (type->type_kind)
	{
		case TYPE_ARRAY:
		{
			LLVMTypeRef array_type = llvm_get_type(c, type);
			for (unsigned i = 0; i < type->array.len; i++)
			{
				AlignSize element_align;
				LLVMValueRef target = llvm_emit_array_gep_raw(c, ref, array_type, i, alignment, &element_align);
				llvm_expand_from_args(c, type->array.base, target, index, element_align);
			}
			return;
		}
		case TYPE_STRUCT:
		{
			LLVMTypeRef struct_type = llvm_get_type(c, type);
			FOREACH_IDX(i, Decl *, member, type->decl->strukt.members)
			{
				AlignSize element_align;
				LLVMValueRef target = llvm_emit_struct_gep_raw(c, ref, struct_type, i, alignment, &element_align);
				llvm_expand_from_args(c, member->type, target, index, element_align);
			}
			return;
		}
		case TYPE_UNION:
		{
			Type *largest_type = type_find_largest_union_element(type);
			// COERCE UPDATE bitcast removed, check for ways to optimize
			llvm_expand_from_args(c, largest_type, ref, index, alignment);
			return;
		}
		default:
			llvm_store_to_ptr_raw_aligned(c, ref, llvm_get_next_param(c, index), alignment);
			return;
	}
}

LLVMValueRef llvm_get_next_param(GenContext *c, unsigned *index)
{
	return LLVMGetParam(c->cur_func.ref, (*index)++);
}


static inline void llvm_process_parameter_value(GenContext *c, Decl *decl, ABIArgInfo *info, unsigned *index)
{
	switch (info->kind)
	{
		case ABI_ARG_IGNORE:
			return;
		case ABI_ARG_INDIRECT:
			// Indirect is caller copied.
			decl->backend_ref = llvm_get_next_param(c, index);
			return;
		case ABI_ARG_EXPAND_COERCE:
		{
			llvm_emit_and_set_decl_alloca(c, decl);
			LLVMValueRef addr = decl->backend_ref;
			AlignSize align = decl->alignment;
			llvm_store_to_ptr_raw_aligned(c, addr, llvm_get_next_param(c, index), align);
			(void)llvm_coerce_expand_hi_offset(c, &addr, info, &align);
			llvm_store_to_ptr_raw_aligned(c, addr, llvm_get_next_param(c, index), align);
			break;
		}
		case ABI_ARG_DIRECT_PAIR:
		{
			// Create the two abi types.
			LLVMTypeRef lo = llvm_abi_type(c, info->direct_pair.lo);
			LLVMTypeRef hi = llvm_abi_type(c, info->direct_pair.hi);

			AlignSize decl_alignment = decl->alignment;
			AlignSize hi_alignment = llvm_abi_alignment(c, hi);
			AlignSize lo_alignment = llvm_abi_alignment(c, lo);
			ByteSize hi_aligned_size = aligned_offset(llvm_store_size(c, hi), hi_alignment);
			AlignSize pref_align = MAX(hi_alignment, lo_alignment);

			// Realign to best alignment.
			if (pref_align > decl_alignment) decl_alignment = decl->alignment = pref_align;
			AlignSize hi_offset = aligned_offset(llvm_store_size(c, lo), hi_alignment);
			ASSERT0(hi_offset + llvm_store_size(c, hi) <= type_size(decl->type));

			// Emit decl
			llvm_emit_and_set_decl_alloca(c, decl);
			LLVMValueRef addr = decl->backend_ref;

			// Store it in the lo position.
			llvm_store_to_ptr_raw_aligned(c, addr, llvm_get_next_param(c, index), decl_alignment);

			// Calculate the address
			addr = llvm_emit_pointer_inbounds_gep_raw(c, hi, addr, llvm_const_int(c, type_usz, hi_offset / hi_aligned_size));

			// Store it in the hi location
			llvm_store_to_ptr_raw_aligned(c, addr, llvm_get_next_param(c, index), type_min_alignment(decl_alignment, hi_offset));
			return;
		}
		case ABI_ARG_DIRECT:
	DIRECT_FROM_COERCE:
		{
			LLVMValueRef param_value = llvm_get_next_param(c, index);
			if (decl->var.not_null && safe_mode_enabled())
			{
				LLVMValueRef is_null = LLVMBuildIsNull(c->builder, param_value, "");
				scratch_buffer_clear();
				if (decl->name)
				{
					scratch_buffer_printf("Reference parameter '%s' was passed a null pointer argument.", decl->name);
				}
				else
				{
					// This is currently not possible, but let's handle it anyway.
					scratch_buffer_append("A null pointer argument was passed to a '&' parameter.");
				}
				llvm_emit_panic_on_true(c,
										is_null,
										scratch_buffer_to_string(),
										decl->span,
										NULL, NULL, NULL);
			}
			if (!decl->var.is_written && !decl->var.is_addr && !llvm_use_accurate_debug_info(c))
			{
				decl->backend_value = param_value;
				decl->is_value = true;
				return;
			}
			llvm_emit_and_set_decl_alloca(c, decl);
			llvm_store_decl_raw(c, decl, param_value);
			return;
		}
		case ABI_ARG_DIRECT_SPLIT_STRUCT_I32:
		{
			// In this case we've been flattening the parameter into multiple registers.
			LLVMTypeRef coerce_type = llvm_get_coerce_type(c, info);
			llvm_emit_and_set_decl_alloca(c, decl);

			// Cast to the coerce type.
			// COERCE UPDATE bitcast removed, check for ways to optimize
			LLVMValueRef cast = decl->backend_ref;

			AlignSize decl_alignment = decl->alignment;
			// Store each expanded parameter.
			for (unsigned idx = 0; idx < info->direct_struct_expand; idx++)
			{
				AlignSize align;
				LLVMValueRef element_ptr = llvm_emit_struct_gep_raw(c, cast, coerce_type, idx, decl_alignment, &align);
				LLVMValueRef value = llvm_get_next_param(c, index);
				llvm_store_to_ptr_raw_aligned(c, element_ptr, value, align);
			}
			return;
		}
		case ABI_ARG_DIRECT_COERCE:
		{
			LLVMTypeRef coerce_type = llvm_get_type(c, info->direct_coerce_type);
			if (coerce_type == llvm_get_type(c, decl->type))
			{
				goto DIRECT_FROM_COERCE;
			}
			llvm_emit_and_set_decl_alloca(c, decl);

			LLVMValueRef param = llvm_get_next_param(c, index);
			// Store it with the alignment of the decl.
			llvm_emit_coerce_store(c, decl->backend_ref, decl->alignment, coerce_type, param, llvm_get_type(c, decl->type));
			return;
		}
		case ABI_ARG_DIRECT_COERCE_INT:
		{
			LLVMTypeRef coerce_type = LLVMIntTypeInContext(c->context, type_size(decl->type) * 8);
			if (coerce_type == llvm_get_type(c, decl->type))
			{
				goto DIRECT_FROM_COERCE;
			}
			llvm_emit_and_set_decl_alloca(c, decl);

			LLVMValueRef param = llvm_get_next_param(c, index);
			// Store it with the alignment of the decl.
			llvm_emit_coerce_store(c, decl->backend_ref, decl->alignment, coerce_type, param, llvm_get_type(c, decl->type));
			return;
		}
		case ABI_ARG_EXPAND:
		{
			llvm_emit_and_set_decl_alloca(c, decl);
			llvm_expand_from_args(c, decl->type, decl->backend_ref, index, decl->alignment);
		}
	}
}
static inline void llvm_emit_func_parameter(GenContext *context, Decl *decl, ABIArgInfo *abi_info, unsigned *index, unsigned real_index)
{
	ASSERT0(decl->decl_kind == DECL_VAR && decl->var.kind == VARDECL_PARAM);

	// Allocate room on stack, but do not copy.
	llvm_process_parameter_value(context, decl, abi_info, index);
	if (llvm_use_debug(context))
	{
		llvm_emit_debug_parameter(context, decl, real_index);
	}
}

static inline void llvm_emit_return_value(GenContext *context, LLVMValueRef value)
{
	if (!value)
	{
		LLVMBuildRetVoid(context->builder);
	}
	else
	{
		LLVMBuildRet(context->builder, value);
	}
	context->current_block = NULL;
}


void llvm_emit_return_abi(GenContext *c, BEValue *return_value, BEValue *optional)
{
	FunctionPrototype *prototype = c->cur_func.prototype;

	// If there is no prototype, this is a static initializer, so bail.
	if (!prototype)
	{
		llvm_emit_return_value(c, NULL);
		return;
	}

	ABIArgInfo *info = prototype->ret_abi_info;

	// If we have an optional it's always the return argument, so we need to copy
	// the return value into the return value holder.
	LLVMValueRef return_out = c->return_out;
	Type *call_return_type = prototype->abi_ret_type;

	BEValue no_fail;

	// In this case we use the optional as the actual return.
	if (prototype->is_optional)
	{
		if (return_value && return_value->type != type_void)
		{
			ASSERT0(return_value->value);
			llvm_store_to_ptr_aligned(c, c->return_out, return_value, type_alloca_alignment(return_value->type));
		}
		return_out = c->optional_out;
		if (!optional)
		{
			llvm_value_set(&no_fail, llvm_get_zero(c, type_anyfault), type_anyfault);
			optional = &no_fail;
		}
		return_value = optional;
	}
	ASSERT0(return_value || info->kind == ABI_ARG_IGNORE);

	switch (info->kind)
	{
		case ABI_ARG_INDIRECT:
			ASSERT0(return_value);
			llvm_store_to_ptr_aligned(c, return_out, return_value, info->indirect.alignment);
			llvm_emit_return_value(c, NULL);
			return;
		case ABI_ARG_IGNORE:
			llvm_emit_return_value(c, NULL);
			return;
		case ABI_ARG_DIRECT_SPLIT_STRUCT_I32:
		case ABI_ARG_EXPAND:
			// Expands to multiple slots -
			// Not applicable to return values.
			UNREACHABLE
		case ABI_ARG_EXPAND_COERCE:
		{
			// Pick the return as an address.
			llvm_value_addr(c, return_value);
			LLVMValueRef addr = return_value->value;
			AlignSize align = return_value->alignment;
			LLVMValueRef lo = llvm_load(c, llvm_get_type(c, info->coerce_expand.lo), addr, align, "");
			LLVMTypeRef type2 = llvm_coerce_expand_hi_offset(c, &addr, info, &align);
			LLVMValueRef hi = llvm_load(c, type2, addr, align, "");
			LLVMTypeRef type = llvm_get_twostruct(c, LLVMTypeOf(lo), LLVMTypeOf(hi));
			LLVMValueRef composite = llvm_get_undef_raw(type);

			composite = llvm_emit_insert_value(c, composite, lo, 0);
			composite = llvm_emit_insert_value(c, composite, hi, 1);

			// And return that unpadded result
			llvm_emit_return_value(c, composite);
			break;
		}
		case ABI_ARG_DIRECT:
DIRECT_RETURN:
			// The normal return
			llvm_emit_return_value(c, llvm_load_value_store(c, return_value));
			return;
		case ABI_ARG_DIRECT_PAIR:
		{
			LLVMTypeRef coerce_type = llvm_get_coerce_type(c, info);
			if (coerce_type == llvm_get_type(c, call_return_type)) goto DIRECT_RETURN;
			llvm_emit_return_value(c, llvm_emit_coerce(c, coerce_type, return_value, call_return_type));
			return;
		}
		case ABI_ARG_DIRECT_COERCE_INT:
		{
			LLVMTypeRef coerce_type = LLVMIntTypeInContext(c->context, type_size(call_return_type) * 8);
			if (coerce_type == llvm_get_type(c, call_return_type)) goto DIRECT_RETURN;
			llvm_emit_return_value(c, llvm_emit_coerce(c, coerce_type, return_value, call_return_type));
			return;
		}
		case ABI_ARG_DIRECT_COERCE:
		{
			LLVMTypeRef coerce_type = llvm_get_type(c, info->direct_coerce_type);
			if (coerce_type == llvm_get_type(c, call_return_type)) goto DIRECT_RETURN;
			llvm_emit_return_value(c, llvm_emit_coerce(c, coerce_type, return_value, call_return_type));
			return;
		}
	}
}

void llvm_emit_return_implicit(GenContext *c)
{
	Type *rtype_real = c->cur_func.prototype ? c->cur_func.prototype->rtype : type_void;
	if (type_lowering(type_no_optional(rtype_real)) != type_void)
	{
		LLVMBuildUnreachable(c->builder);
		return;
	}
	if (type_is_optional(rtype_real))
	{
		llvm_emit_return_abi(c, NULL, NULL);
		return;
	}
	BEValue value;
	llvm_value_set(&value, llvm_get_zero(c, type_anyfault), type_anyfault);
	llvm_emit_return_abi(c, NULL, &value);
}

void llvm_emit_function_body(GenContext *c, Decl *decl)
{
	DEBUG_LOG("Generating function %s.", decl->name);
	if (decl->func_decl.attr_dynamic) vec_add(c->dynamic_functions, decl);
	ASSERT0(decl->backend_ref);
	if (decl->func_decl.attr_init || (decl->func_decl.attr_finalizer && compiler.platform.object_format == OBJ_FORMAT_MACHO))
	{
		llvm_append_xxlizer(c, decl->func_decl.priority, decl->func_decl.attr_init, decl->backend_ref);
	}
	if (decl->func_decl.attr_finalizer && compiler.platform.object_format != OBJ_FORMAT_MACHO)
	{
		LLVMValueRef atexit = LLVMGetNamedFunction(c->module, "atexit");
		if (!atexit) atexit = LLVMAddFunction(c->module, "atexit", c->atexit_type);
		scratch_buffer_clear();
		scratch_buffer_append(".__c3_atexit_");
		scratch_buffer_set_extern_decl_name(decl, false);
		LLVMValueRef func = LLVMAddFunction(c->module, scratch_buffer_to_string(), c->xtor_func_type);
		llvm_set_weak(c, func);
		LLVMBuilderRef builder = llvm_create_function_entry(c, func, NULL);
		LLVMValueRef args[1] = { decl->backend_ref };
		LLVMBuildCall2(builder, c->atexit_type, atexit, args, 1, "");
		LLVMBuildRetVoid(builder);
		LLVMDisposeBuilder(builder);
		llvm_append_xxlizer(c, decl->func_decl.priority, true, func);
	}
	llvm_emit_body(c,
	               decl->backend_ref,
	               type_get_resolved_prototype(decl->type),
	               decl->func_decl.attr_naked ? NULL : &decl->func_decl.signature,
	               astptr(decl->func_decl.body), decl);
}


void llvm_emit_body(GenContext *c, LLVMValueRef function, FunctionPrototype *prototype, Signature *signature, Ast *body,
                    Decl *decl)
{
	ASSERT0(prototype && function && body);
	// Signature is NULL if the function is naked.

	bool emit_debug = llvm_use_debug(c);
	LLVMValueRef prev_function = c->cur_func.ref;
	LLVMBuilderRef prev_builder = c->builder;

	c->catch = NO_CATCH;

	if (emit_debug)
	{
		c->debug.function = LLVMGetSubprogram(function);
	}

	c->panic_blocks = NULL;
	c->cur_func.ref = function;
	c->cur_func.name = decl->name;
	c->cur_func.prototype = prototype;
	c->builder = llvm_create_function_entry(c, function, &c->current_block);
	c->first_block = c->current_block;

	LLVMValueRef alloca_point = LLVMBuildAlloca(c->builder, LLVMInt32TypeInContext(c->context), "alloca_point");
	c->alloca_point = alloca_point;

	unsigned arg = 0;


	DebugScope scope;
	if (emit_debug)
	{
		scope = (DebugScope) { .lexical_block = c->debug.function, NULL, NULL };
		c->debug.block_stack = &scope;
		EMIT_LOC(c, body);
	}

	c->optional_out = NULL;
	c->return_out = NULL;
	if (prototype->ret_abi_info->kind == ABI_ARG_INDIRECT)
	{
		if (prototype->is_optional)
		{
			c->optional_out = llvm_get_next_param(c, &arg);
		}
		else
		{
			c->return_out = llvm_get_next_param(c, &arg);
		}
	}
	if (prototype->ret_by_ref_abi_info)
	{
		ASSERT0(!c->return_out);
		c->return_out = llvm_get_next_param(c, &arg);
	}


	if (signature)
	{
		// Generate LLVMValueRef's for all parameters, so we can use them as local vars in code
		FOREACH_IDX(i, Decl *, param, signature->params)
		{
			llvm_emit_func_parameter(c, param, prototype->abi_args[i], &arg, i);
		}
	}

	LLVMSetCurrentDebugLocation2(c->builder, NULL);

	AstId current = body->compound_stmt.first_stmt;
	while (current)
	{
		llvm_emit_stmt(c, ast_next(&current));
	}

	llvm_delete_current_if_unused(c);

	// Insert a return (and defer) if needed.
	if (c->current_block && !LLVMGetBasicBlockTerminator(c->current_block))
	{
		llvm_emit_return_implicit(c);
	}

	LLVMBasicBlockRef last_block = LLVMGetLastBasicBlock(c->cur_func.ref);

	// Move panic blocks last, this is just overall nicer to read, and might be better from
	// a performance POV
	FOREACH(LLVMBasicBlockRef, panic_block, c->panic_blocks)
	{
		if (last_block == panic_block) continue;
		LLVMMoveBasicBlockAfter(panic_block, last_block);
		last_block = panic_block;
	}

	// erase alloca point
	if (LLVMGetInstructionParent(alloca_point))
	{
		c->alloca_point = NULL;
		LLVMInstructionEraseFromParent(alloca_point);
	}

	LLVMDisposeBuilder(c->builder);
	c->builder = c->global_builder;

	if (llvm_use_debug(c))
	{
		c->debug.block_stack = NULL;
		LLVMDIBuilderFinalizeSubprogram(c->debug.builder, c->debug.function);
	}

	c->builder = prev_builder;
	c->cur_func.ref = prev_function;
}

static void llvm_append_xxlizer(GenContext *c, unsigned  priority, bool is_initializer, LLVMValueRef function)
{
	LLVMValueRef **array_ref = is_initializer ? &c->constructors : &c->destructors;
	LLVMValueRef vals[3] = { llvm_const_int(c, type_int, priority), function, llvm_get_zero(c, type_voidptr) };
	vec_add(*array_ref, LLVMConstNamedStruct(c->xtor_entry_type, vals, 3));
}


void llvm_emit_dynamic_functions(GenContext *c, Decl **funcs)
{
	size_t len = vec_size(funcs);
	if (!len) return;
	if (compiler.platform.object_format == OBJ_FORMAT_MACHO)
	{
		LLVMTypeRef types[3] = { c->ptr_type, c->ptr_type, c->typeid_type };
		LLVMTypeRef entry_type = LLVMStructType(types, 3, false);
		LLVMValueRef *entries = VECNEW(LLVMValueRef, len);
		FOREACH(Decl *, func, funcs)
		{
			Type *type = typeget(func->func_decl.type_parent);
			Decl *proto = declptrzero(func->func_decl.interface_method);
			LLVMValueRef proto_ref = proto ? llvm_get_ref(c, proto) : llvm_get_selector(c, func->name);
			LLVMValueRef vals[3] = {llvm_get_ref(c, func), proto_ref, llvm_get_typeid(c, type)};
			LLVMValueRef entry = LLVMConstNamedStruct(entry_type, vals, 3);
			vec_add(entries, entry);
		}
		LLVMValueRef array = LLVMConstArray(entry_type, entries, len);
		LLVMValueRef global = LLVMAddGlobal(c->module, LLVMTypeOf(array), "$c3_dynamic");
		LLVMSetLinkage(global, LLVMInternalLinkage);
		LLVMSetInitializer(global, array);
		LLVMSetSection(global, "__DATA,__c3_dynamic");
		LLVMSetAlignment(global, llvm_abi_alignment(c, c->xtor_entry_type));
		return;
	}

	LLVMValueRef initializer = LLVMAddFunction(c->module, ".c3_dynamic_register", c->xtor_func_type);
	LLVMSetLinkage(initializer, LLVMInternalLinkage);
	LLVMSetAlignment(initializer, 8);
	LLVMValueRef vals_fn[3] = { llvm_const_int(c, type_int, 1), initializer, llvm_get_zero(c, type_voidptr) };
	vec_add(c->constructors, LLVMConstNamedStruct(c->xtor_entry_type, vals_fn, 3));

	LLVMBasicBlockRef last_block;
	LLVMBuilderRef builder = llvm_create_function_entry(c, initializer, &last_block);
	FOREACH(Decl *, decl, funcs)
	{
		Type *type = typeget(decl->func_decl.type_parent);
		scratch_buffer_clear();
		scratch_buffer_append("$ct.dyn.");
		scratch_buffer_set_extern_decl_name(decl, false);
		LLVMValueRef global = llvm_add_global_raw(c, scratch_buffer_copy(), c->dtable_type, 0);
		llvm_set_weak(c, global);
		Decl *proto = declptrzero(decl->func_decl.interface_method);
		LLVMValueRef proto_ref = proto ? llvm_get_ref(c, proto) : llvm_get_selector(c, decl->name);

		LLVMValueRef all_one_ptr = LLVMConstAllOnes(llvm_get_type(c, type_uptr));
		all_one_ptr = LLVMBuildIntToPtr(builder, all_one_ptr, c->ptr_type, "");
		LLVMValueRef vals[3] = {llvm_get_ref(c, decl), proto_ref, all_one_ptr};
		LLVMSetInitializer(global, LLVMConstNamedStruct(c->dtable_type, vals, 3));

		LLVMBasicBlockRef check = llvm_basic_block_new(c, "dtable_check");
		LLVMBasicBlockRef skip = llvm_basic_block_new(c, "dtable_skip");

		LLVMValueRef check_ptr = LLVMBuildStructGEP2(builder, c->dtable_type, global, 2, "check_dtable_ref");
		LLVMValueRef load_check = LLVMBuildLoad2(builder, c->ptr_type, check_ptr, "next_val");
		LLVMBuildCondBr(builder, LLVMBuildICmp(builder, LLVMIntEQ, load_check, all_one_ptr, ""), check, skip);

		LLVMAppendExistingBasicBlock(initializer, check);
		LLVMPositionBuilderAtEnd(builder, check);

		// Pointer to table
		LLVMValueRef type_id_ptr = LLVMBuildIntToPtr(builder, llvm_get_typeid(c, type), c->ptr_type, "");
		LLVMValueRef dtable_ref = LLVMBuildStructGEP2(builder, c->introspect_type, type_id_ptr, INTROSPECT_INDEX_DTABLE, "introspect_index");

		// Phi is dtable**
		LLVMValueRef phi = LLVMBuildPhi(builder, c->ptr_type, "dtable_ref");
		LLVMAddIncoming(phi, &dtable_ref, &last_block, 1);

		// Load Phi to dtable*
		LLVMValueRef load_dtable = LLVMBuildLoad2(builder, c->ptr_type, phi, "dtable_ptr");

		// Check if null
		LLVMValueRef is_null = LLVMBuildICmp(builder, LLVMIntEQ, load_dtable, LLVMConstNull(c->ptr_type), "");
		LLVMValueRef next_ptr = LLVMBuildStructGEP2(builder, c->dtable_type, load_dtable, 2, "next_dtable_ref");
		// Grab new pointer
		LLVMAddIncoming(phi, &next_ptr, &check, 1);

		LLVMBasicBlockRef after_check = llvm_basic_block_new(c, "dtable_found");
		LLVMBuildCondBr(builder, is_null, after_check, check);

		// We have a dtable** which points to a null
		LLVMAppendExistingBasicBlock(initializer, after_check);
		LLVMPositionBuilderAtEnd(builder, after_check);

		// Store the global (dtable*) to the phi (dtable**)
		LLVMBuildStore(builder, global, phi);

		// Clear the -1
		LLVMBuildStore(builder, LLVMConstNull(c->ptr_type), check_ptr);

		// Goto the skip
		LLVMBuildBr(builder, skip);
		LLVMAppendExistingBasicBlock(initializer, skip);
		LLVMPositionBuilderAtEnd(builder, skip);
		last_block = skip;
	}

	LLVMBuildRet(builder, NULL);
	LLVMDisposeBuilder(builder);
}

void llvm_emit_function_decl(GenContext *c, Decl *decl)
{
	ASSERT0(decl->decl_kind == DECL_FUNC);
	// Resolve function backend type for function.
	decl_append_links_to_global(decl);
	LLVMValueRef function = llvm_get_ref(c, decl);
	decl->backend_ref = function;
	if (decl->attrs_resolved && decl->attrs_resolved->section)
	{
		LLVMSetSection(function, decl->attrs_resolved->section);
	}
	if (llvm_use_debug(c))
	{
		llvm_emit_debug_function(c, decl);
	}
}


