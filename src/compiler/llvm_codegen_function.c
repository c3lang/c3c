// Copyright (c) 2019 Christoffer Lerno. All rights reserved.
// Use of this source code is governed by the GNU LGPLv3.0 license
// a copy of which can be found in the LICENSE file.


#include "llvm_codegen_internal.h"

static LLVMValueRef llvm_add_xxlizer(GenContext *c, unsigned priority, bool is_finalizer);
static void llvm_emit_param_attributes(GenContext *c, LLVMValueRef function, ABIArgInfo *info, bool is_return, int index, int last_index);
static inline void llvm_emit_return_value(GenContext *context, LLVMValueRef value);
static void llvm_expand_from_args(GenContext *c, Type *type, LLVMValueRef ref, unsigned *index, AlignSize alignment);
static inline void llvm_process_parameter_value(GenContext *c, Decl *decl, ABIArgInfo *info, unsigned *index);
static inline void llvm_emit_func_parameter(GenContext *context, Decl *decl, ABIArgInfo *abi_info, unsigned *index, unsigned real_index);
static inline void llvm_emit_body(GenContext *c, LLVMValueRef function, const char *module_name,
								  const char *function_name,
								  FileId file_id, FunctionPrototype *prototype, Signature *signature, Ast *body);


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
	if (!c->current_block_is_target
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


void llvm_emit_block(GenContext *context, LLVMBasicBlockRef next_block)
{
	assert(context->current_block == NULL);
	LLVMAppendExistingBasicBlock(context->function, next_block);
	LLVMPositionBuilderAtEnd(context->builder, next_block);
	context->current_block = next_block;
	context->current_block_is_target = false;
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
			Decl **members = type->decl->strukt.members;
			VECEACH(members, i)
			{
				AlignSize element_align;
				LLVMValueRef target = llvm_emit_struct_gep_raw(c, ref, struct_type, i, alignment, &element_align);
				llvm_expand_from_args(c, members[i]->type, target, index, element_align);
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

LLVMValueRef llvm_get_next_param(GenContext *context, unsigned *index)
{
	return LLVMGetParam(context->function, (*index)++);
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
			assert(hi_offset + llvm_store_size(c, hi) <= type_size(decl->type));

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
			if (decl->var.not_null && active_target.feature.safe_mode)
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
			if (!decl->var.is_written && !decl->var.is_addr)
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
	assert(decl->decl_kind == DECL_VAR && decl->var.kind == VARDECL_PARAM);

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
	context->current_block_is_target = false;
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
			assert(return_value->value);
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
	assert(return_value || info->kind == ABI_ARG_IGNORE);

	switch (info->kind)
	{
		case ABI_ARG_INDIRECT:
			assert(return_value);
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
	DEBUG_LOG("Generating function %s.", decl->extname);
	if (decl->func_decl.attr_dynamic) vec_add(c->dynamic_functions, decl);
	assert(decl->backend_ref);
	llvm_emit_body(c,
				   decl->backend_ref,
				   decl->unit->module->name->module,
				   decl->name,
				   decl->span.file_id,
				   type_get_resolved_prototype(decl->type),
				   decl->func_decl.attr_naked ? NULL : &decl->func_decl.signature,
				   astptr(decl->func_decl.body));
}

void llvm_emit_stacktrace_definitions(GenContext *c)
{
	const char *name = ".stacktrace_current";
	LLVMValueRef current_stack = c->debug.current_stack_ptr = llvm_add_global_raw(c, name, c->ptr_type, 0);
	LLVMSetThreadLocal(current_stack, true);
	LLVMSetInitializer(current_stack, llvm_get_zero_raw(c->ptr_type));
	llvm_set_weak(c, current_stack);
	LLVMTypeRef uint_type = llvm_get_type(c, type_uint);
	LLVMTypeRef args[6] = { c->ptr_type, c->ptr_type, c->size_type, c->ptr_type, c->size_type, uint_type };
	LLVMTypeRef func_type = c->debug.stack_init_fn_type = LLVMFunctionType(LLVMVoidTypeInContext(c->context), args, 6, false);
	LLVMValueRef func = c->debug.stack_init_fn = LLVMAddFunction(c->module, ".stacktrace_init", func_type);
	llvm_set_weak(c, func);
	LLVMBuilderRef builder = LLVMCreateBuilderInContext(c->context);
	LLVMBasicBlockRef entry = LLVMAppendBasicBlockInContext(c->context, func, "entry");
	LLVMPositionBuilderAtEnd(builder, entry);
	c->builder = builder;
	LLVMValueRef stacktrace = LLVMGetParam(func, 0);
	AlignSize align_to_use;
	LLVMTypeRef slot_type = c->debug.stack_type;
	AlignSize alignment = llvm_abi_alignment(c, slot_type);
	LLVMValueRef prev_ptr = llvm_emit_struct_gep_raw(c, stacktrace, slot_type, 0, alignment, &align_to_use);
	llvm_store_to_ptr_raw_aligned(c, prev_ptr,
								  LLVMBuildLoad2(c->builder, c->ptr_type, c->debug.current_stack_ptr, ""),
								  align_to_use);
	LLVMValueRef func_name = llvm_emit_struct_gep_raw(c, stacktrace, slot_type, 1, alignment, &align_to_use);
	LLVMValueRef func_name_ptr = llvm_emit_struct_gep_raw(c, func_name, c->chars_type, 0, align_to_use, &align_to_use);
	llvm_store_to_ptr_raw_aligned(c, func_name_ptr, LLVMGetParam(func, 1), align_to_use);
	LLVMValueRef func_name_sz = llvm_emit_struct_gep_raw(c, func_name, c->chars_type, 1, align_to_use, &align_to_use);
	llvm_store_to_ptr_raw_aligned(c, func_name_sz, LLVMGetParam(func, 2), align_to_use);

	LLVMValueRef file_name = llvm_emit_struct_gep_raw(c, stacktrace, slot_type, 2, alignment, &align_to_use);
	LLVMValueRef file_name_ptr = llvm_emit_struct_gep_raw(c, file_name, c->chars_type, 0, align_to_use, &align_to_use);
	llvm_store_to_ptr_raw_aligned(c, file_name_ptr, LLVMGetParam(func, 3), align_to_use);
	LLVMValueRef file_name_sz = llvm_emit_struct_gep_raw(c, file_name, c->chars_type, 1, align_to_use, &align_to_use);
	llvm_store_to_ptr_raw_aligned(c, file_name_sz, LLVMGetParam(func, 4), align_to_use);
	llvm_store_to_ptr_raw_aligned(c,
								  c->debug.current_stack_ptr,
								  stacktrace,
								  type_alloca_alignment(type_voidptr));
	LLVMValueRef line = llvm_emit_struct_gep_raw(c, stacktrace, slot_type, 3, alignment, &align_to_use);
	llvm_store_to_ptr_raw_aligned(c, line, LLVMGetParam(func, 5), align_to_use);
	LLVMBuildRetVoid(c->builder);
	LLVMDisposeBuilder(c->builder);
	c->builder = NULL;
}

void llvm_emit_body(GenContext *c, LLVMValueRef function, const char *module_name, const char *function_name,
					FileId file_id, FunctionPrototype *prototype, Signature *signature, Ast *body)
{
	bool emit_debug = llvm_use_debug(c);
	LLVMValueRef prev_function = c->function;
	LLVMBuilderRef prev_builder = c->builder;

	bool use_stacktrace = emit_debug && c->debug.enable_stacktrace;
	if (use_stacktrace && !c->debug.stack_init_fn)
	{
		llvm_emit_stacktrace_definitions(c);
		c->builder = prev_builder;
	}
	c->opt_var = NULL;
	c->catch_block = NULL;

	c->function = function;
	if (!function_name) function_name = "anonymous function";
	size_t func_name_len = 0;
	size_t file_name_len = 0;
	if (emit_debug)
	{
		c->debug.function = LLVMGetSubprogram(function);
		if (use_stacktrace)
		{
			scratch_buffer_clear();
			scratch_buffer_append(module_name);
			scratch_buffer_append("::");
			scratch_buffer_append(function_name);
			c->debug.func_name = llvm_emit_zstring_named(c, scratch_buffer_to_string(), ".funcname");
			func_name_len = scratch_buffer.len;
			File *file = source_file_by_id(file_id);
			file_name_len = strlen(file->name);
			c->debug.file_name = llvm_emit_zstring_named(c, file->name, ".filename");
		}
	}

	c->cur_func.name = function_name;
	c->cur_func.prototype = prototype;
	LLVMBasicBlockRef entry = LLVMAppendBasicBlockInContext(c->context, c->function, "entry");
	c->current_block = entry;
	c->current_block_is_target = true;
	c->builder = LLVMCreateBuilderInContext(c->context);
	LLVMPositionBuilderAtEnd(c->builder, entry);

	LLVMValueRef alloca_point = LLVMBuildAlloca(c->builder, LLVMInt32TypeInContext(c->context), "alloca_point");
	c->alloca_point = alloca_point;

	unsigned arg = 0;

	if (emit_debug)
	{
		llvm_debug_scope_push(c, c->debug.function);
		EMIT_LOC(c, body);
		if (use_stacktrace)
		{
			LLVMTypeRef slot_type = c->debug.stack_type;
			assert(c->debug.current_stack_ptr && c->debug.stack_init_fn);
			AlignSize alignment = llvm_abi_alignment(c, slot_type);
			LLVMValueRef stacktrace = c->debug.stack_slot = llvm_emit_alloca(c, slot_type, alignment, ".$stacktrace");
			LLVMValueRef args[] = { stacktrace, c->debug.func_name, llvm_const_int(c, type_usz, func_name_len),
									c->debug.file_name, llvm_const_int(c, type_usz, file_name_len),
									llvm_const_int(c, type_uint, body->span.row) };
			LLVMBuildCall2(c->builder, c->debug.stack_init_fn_type, c->debug.stack_init_fn, args, 6, "");
			c->debug.stack_slot_row = LLVMBuildStructGEP2(c->builder, slot_type, c->debug.stack_slot, 3, ".$row");
			if (function_name == kw_main || function_name == kw_mainstub)
			{
				AlignSize align_size;
				LLVMValueRef last = llvm_emit_struct_gep_raw(c, c->debug.stack_slot, slot_type, 0, alignment, &align_size);
				llvm_store_to_ptr_raw_aligned(c, last, LLVMConstNull(c->ptr_type), align_size);
			}
		}
	}

	c->optional_out = NULL;
	c->return_out = NULL;
	if (prototype && prototype->ret_abi_info->kind == ABI_ARG_INDIRECT)
	{
		if (prototype->is_optional)
		{
			c->optional_out = LLVMGetParam(c->function, arg++);
		}
		else
		{
			c->return_out = LLVMGetParam(c->function, arg++);
		}
	}
	if (prototype && prototype->ret_by_ref_abi_info)
	{
		assert(!c->return_out);
		c->return_out = LLVMGetParam(c->function, arg++);
	}


	if (signature)
	{
		// Generate LLVMValueRef's for all parameters, so we can use them as local vars in code
		FOREACH_BEGIN_IDX(i, Decl *param, signature->params)
			llvm_emit_func_parameter(c, param, prototype->abi_args[i], &arg, i);
		FOREACH_END();
	}

	LLVMSetCurrentDebugLocation2(c->builder, NULL);

	AstId current = body->compound_stmt.first_stmt;
	while (current)
	{
		llvm_emit_stmt(c, ast_next(&current));
	}

	if (c->current_block && llvm_basic_block_is_unused(c->current_block))
	{
		LLVMBasicBlockRef prev_block = LLVMGetPreviousBasicBlock(c->current_block);
		LLVMDeleteBasicBlock(c->current_block);
		c->current_block = prev_block;
		LLVMPositionBuilderAtEnd(c->builder, c->current_block);
	}
	// Insert a return (and defer) if needed.
	if (c->current_block && !LLVMGetBasicBlockTerminator(c->current_block))
	{
		llvm_emit_return_implicit(c);
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
		llvm_debug_scope_pop(c);
		LLVMDIBuilderFinalizeSubprogram(c->debug.builder, c->debug.function);
	}

	c->builder = prev_builder;
	c->function = prev_function;
}

static LLVMValueRef llvm_add_xxlizer(GenContext *c, unsigned priority, bool is_initializer)
{
	LLVMTypeRef initializer_type = LLVMFunctionType(LLVMVoidTypeInContext(c->context), NULL, 0, false);
	LLVMValueRef **array_ref = is_initializer ? &c->constructors : &c->destructors;
	scratch_buffer_clear();
	scratch_buffer_printf(is_initializer ? ".static_initialize.%u" : ".static_finalize.%u", vec_size(*array_ref));
	LLVMValueRef function = LLVMAddFunction(c->module, scratch_buffer_to_string(), initializer_type);
	LLVMSetLinkage(function, LLVMInternalLinkage);
	LLVMValueRef vals[3] = { llvm_const_int(c, type_int, priority), function, llvm_get_zero(c, type_voidptr) };
	vec_add(*array_ref, LLVMConstStructInContext(c->context, vals, 3, false));
	return function;
}

void llvm_emit_xxlizer(GenContext *c, Decl *decl)
{
	Ast *body = astptrzero(decl->xxlizer.init);
	if (!body)
	{
		// Skip if it doesn't have a body.
		return;
	}
	bool is_initializer = decl->decl_kind == DECL_INITIALIZE;
	LLVMValueRef function = llvm_add_xxlizer(c, decl->xxlizer.priority, is_initializer);
	if (llvm_use_debug(c))
	{
		uint32_t row = decl->span.row;
		if (!row) row = 1;
		LLVMMetadataRef type = LLVMDIBuilderCreateSubroutineType(c->debug.builder, c->debug.file.debug_file, NULL, 0, 0);

		c->debug.function = LLVMDIBuilderCreateFunction(c->debug.builder,
														c->debug.file.debug_file,
														scratch_buffer.str, scratch_buffer.len,
														scratch_buffer.str, scratch_buffer.len,
														c->debug.file.debug_file,
														row,
														type,
														true,
														true,
														row,
														LLVMDIFlagZero,
														active_target.optimization_level != OPTIMIZATION_NONE);
		LLVMSetSubprogram(function, c->debug.function);
	}
	llvm_emit_body(c,
				   function,
				   decl->unit->module->name->module,
				   is_initializer ? "[static initializer]" : "[static finalizer]",
				   decl->span.file_id,
				   NULL,
				   NULL,
				   body);
}


void llvm_emit_dynamic_functions(GenContext *c, Decl **funcs)
{
	if (!vec_size(funcs)) return;
	LLVMValueRef initializer = llvm_add_xxlizer(c, 1, true);
	LLVMBasicBlockRef entry = LLVMAppendBasicBlockInContext(c->context, initializer, "entry");
	LLVMBuilderRef builder = LLVMCreateBuilderInContext(c->context);
	LLVMPositionBuilderAtEnd(builder, entry);
	LLVMBasicBlockRef last_block = entry;
	FOREACH_BEGIN(Decl *decl, funcs)
		Type *type = typeinfotype(decl->func_decl.type_parent);
		scratch_buffer_clear();
		scratch_buffer_append("$ct.dyn.");
		scratch_buffer_append(decl_get_extname(decl));
		LLVMValueRef global = llvm_add_global_raw(c, scratch_buffer_to_string(), c->dtable_type, 0);
		Decl *proto = declptr(decl->func_decl.any_prototype);
		LLVMValueRef proto_ref = llvm_get_ref(c, proto);
		LLVMValueRef vals[3] = { llvm_get_ref(c, decl), proto_ref, LLVMConstNull(c->ptr_type) };
		LLVMSetInitializer(global, LLVMConstNamedStruct(c->dtable_type, vals, 3));
		LLVMValueRef type_id_ptr = LLVMBuildIntToPtr(builder, llvm_get_typeid(c, type), c->ptr_type, "");
		LLVMValueRef dtable_ref = LLVMBuildStructGEP2(builder, c->introspect_type, type_id_ptr, INTROSPECT_INDEX_DTABLE, "");
		LLVMBasicBlockRef check = LLVMAppendBasicBlockInContext(c->context, initializer, "dtable_check");
		LLVMBuildBr(builder, check);
		LLVMPositionBuilderAtEnd(builder, check);
		LLVMValueRef phi = LLVMBuildPhi(builder, c->ptr_type, "dtable_ref");
		LLVMValueRef load_dtable = LLVMBuildLoad2(builder, c->ptr_type, phi, "dtable_ptr");
		LLVMValueRef is_not_null = LLVMBuildICmp(builder, LLVMIntEQ, load_dtable, LLVMConstNull(c->ptr_type), "");
		LLVMBasicBlockRef after_check = llvm_basic_block_new(c, "dtable_found");
		LLVMBasicBlockRef next = llvm_basic_block_new(c, "dtable_next");
		LLVMBuildCondBr(builder, is_not_null, after_check, next);
		LLVMAppendExistingBasicBlock(initializer, next);
		LLVMPositionBuilderAtEnd(builder, next);
		LLVMValueRef next_ptr = LLVMBuildStructGEP2(builder, c->dtable_type, load_dtable, 2, "next_dtable_ref");
		LLVMValueRef phi_in[2] = { dtable_ref, next_ptr };
		LLVMBasicBlockRef phi_in_block[2] = { last_block, next };
		LLVMAddIncoming(phi, phi_in, phi_in_block, 2);
		LLVMBuildBr(builder, check);
		LLVMAppendExistingBasicBlock(initializer, after_check);
		LLVMPositionBuilderAtEnd(builder, after_check);
		LLVMBuildStore(builder, global, phi);
		last_block = after_check;
	FOREACH_END();

	LLVMBuildRet(builder, NULL);
	LLVMDisposeBuilder(builder);
}
void llvm_emit_function_decl(GenContext *c, Decl *decl)
{
	assert(decl->decl_kind == DECL_FUNC);
	// Resolve function backend type for function.

	LLVMValueRef function = llvm_get_ref(c, decl);
	decl->backend_ref = function;
	if (decl->section_id)
	{
		LLVMSetSection(function, section_from_id(decl->section_id));
	}
	if (llvm_use_debug(c))
	{
		llvm_emit_debug_function(c, decl);
	}

	if (decl->is_extern)
	{
		if (decl->is_weak)
		{
			LLVMSetLinkage(function, LLVMExternalWeakLinkage);
			llvm_set_comdat(c, function);
		}
		else
		{
			LLVMSetLinkage(function, LLVMExternalLinkage);
		}
		LLVMSetVisibility(function, LLVMDefaultVisibility);
		return;
	}
	if (decl_is_local(decl))
	{
		LLVMSetLinkage(function, decl->is_weak ? LLVMLinkerPrivateWeakLinkage : LLVMInternalLinkage);
		LLVMSetVisibility(function, LLVMDefaultVisibility);
		return;
	}
	if (decl->is_weak) llvm_set_weak(c, function);
}


