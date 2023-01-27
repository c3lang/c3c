// Copyright (c) 2019 Christoffer Lerno. All rights reserved.
// Use of this source code is governed by the GNU LGPLv3.0 license
// a copy of which can be found in the LICENSE file.


#include "llvm_codegen_internal.h"

static void llvm_emit_param_attributes(GenContext *c, LLVMValueRef function, ABIArgInfo *info, bool is_return, int index, int last_index);
static inline void llvm_emit_return_value(GenContext *context, LLVMValueRef value);
static void llvm_expand_from_args(GenContext *c, Type *type, LLVMValueRef ref, unsigned *index, AlignSize alignment);
static inline void llvm_process_parameter_value(GenContext *c, Decl *decl, ABIArgInfo *info, unsigned *index);
static inline void llvm_emit_func_parameter(GenContext *context, Decl *decl, ABIArgInfo *abi_info, unsigned *index, unsigned real_index);
static inline void llvm_emit_body(GenContext *c, LLVMValueRef function, const char *module_name,
                                  const char *function_name,
                                  FileId file_id, FunctionPrototype *prototype, Signature *signature, Ast *body);

bool llvm_emit_check_block_branch(GenContext *context)
{
	if (!context->current_block) return false;
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
	// Consequently we will delete those and realize that
	// we then have no need for emitting a br.
	if (!context->current_block_is_target
	    && !LLVMGetFirstUse(LLVMBasicBlockAsValue(context->current_block)))
	{
		LLVMDeleteBasicBlock(context->current_block);
		context->current_block = NULL;
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
			LLVMValueRef cast_addr = llvm_emit_bitcast_ptr(c, ref, largest_type);
			llvm_expand_from_args(c, largest_type, cast_addr, index, alignment);
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
			// Create the expand type:
			LLVMTypeRef coerce_type = llvm_get_coerce_type(c, info);
			LLVMValueRef temp = LLVMBuildBitCast(c->builder, decl->backend_ref, LLVMPointerType(coerce_type, 0), "coerce");
			llvm_emit_and_set_decl_alloca(c, decl);

			AlignSize alignment = decl->alignment;
			AlignSize element_align;
			LLVMValueRef gep_first = llvm_emit_struct_gep_raw(c, temp, coerce_type, info->coerce_expand.lo_index, alignment, &element_align);
			llvm_store_to_ptr_raw_aligned(c, gep_first, llvm_get_next_param(c, index), element_align);
			if (abi_type_is_valid(info->coerce_expand.hi))
			{
				LLVMValueRef gep_second = llvm_emit_struct_gep_raw(c, temp, coerce_type, info->coerce_expand.hi_index, alignment, &element_align);
				llvm_store_to_ptr_raw_aligned(c, gep_second, llvm_get_next_param(c, index), element_align);
			}
			break;
		}
		case ABI_ARG_DIRECT_PAIR:
		{
			LLVMTypeRef lo = llvm_abi_type(c, info->direct_pair.lo);
			LLVMTypeRef hi = llvm_abi_type(c, info->direct_pair.hi);
			LLVMTypeRef struct_type = llvm_get_twostruct(c, lo, hi);
			AlignSize decl_alignment = decl->alignment;
			LLVMValueRef coerce;
			if (llvm_store_size(c, struct_type) > type_size(decl->type))
			{
				AlignSize struct_alignment = llvm_abi_alignment(c, struct_type);
				if (decl_alignment < struct_alignment) decl->alignment = decl_alignment = struct_alignment;
				coerce = llvm_emit_alloca(c, struct_type, decl_alignment, "");
				decl->backend_ref = LLVMBuildBitCast(c->builder, coerce, llvm_get_ptr_type(c, decl->type), decl->name ? decl->name : ".anon");
			}
			else
			{
				llvm_emit_and_set_decl_alloca(c, decl);
				// Here we do the following transform:
				// lo, hi -> { lo, hi } -> struct
				// Cast to { lo, hi }
				coerce = LLVMBuildBitCast(c->builder, decl->backend_ref, LLVMPointerType(struct_type, 0), "pair");
			}
			// Point to the lo value.
			AlignSize element_align;
			LLVMValueRef lo_ptr = llvm_emit_struct_gep_raw(c, coerce, struct_type, 0, decl_alignment, &element_align);
			// Store it in the struct.
			llvm_store_to_ptr_raw_aligned(c, lo_ptr, llvm_get_next_param(c, index), element_align);
			// Point to the hi value.
			LLVMValueRef hi_ptr = llvm_emit_struct_gep_raw(c, coerce, struct_type, 1, decl_alignment, &element_align);
			// Store it in the struct.
			llvm_store_to_ptr_raw_aligned(c, hi_ptr, llvm_get_next_param(c, index), element_align);
			return;
		}
		case ABI_ARG_DIRECT:
	DIRECT_FROM_COERCE:
			if (!decl->var.is_written && !decl->var.is_addr)
			{
				decl->backend_value = llvm_get_next_param(c, index);
				decl->is_value = true;
				return;
			}
			llvm_emit_and_set_decl_alloca(c, decl);
			llvm_store_decl_raw(c, decl, llvm_get_next_param(c, index));
			return;
		case ABI_ARG_DIRECT_SPLIT_STRUCT:
		{
			// In this case we've been flattening the parameter into multiple registers.
			LLVMTypeRef coerce_type = llvm_get_coerce_type(c, info);
			llvm_emit_and_set_decl_alloca(c, decl);

			// Cast to the coerce type.
			LLVMValueRef cast = LLVMBuildBitCast(c->builder, decl->backend_ref, LLVMPointerType(coerce_type, 0), "coerce");

			AlignSize decl_alignment = decl->alignment;
			// Store each expanded parameter.
			for (unsigned idx = 0; idx < info->direct_struct_expand.elements; idx++)
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
			if (info->expand.padding_type)
			{
				// Skip the pad.
				llvm_get_next_param(c, index);
			}
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
			llvm_value_set(&no_fail, llvm_get_zero(c, type_anyerr), type_anyerr);
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
		case ABI_ARG_DIRECT_SPLIT_STRUCT:
		case ABI_ARG_EXPAND:
			// Expands to multiple slots -
			// Not applicable to return values.
			UNREACHABLE
		case ABI_ARG_EXPAND_COERCE:
		{
			// Pick the return as an address.
			llvm_value_addr(c, return_value);
			// Get the coerce type.
			LLVMTypeRef coerce_type = llvm_get_coerce_type(c, info);
			// Create the new pointer
			assert(return_value);
			LLVMValueRef coerce = LLVMBuildBitCast(c->builder, return_value->value, coerce_type, "");
			// We might have only one value, in that case, build a GEP to that one.
			LLVMValueRef lo_val;
			AlignSize alignment;
			LLVMValueRef lo = llvm_emit_struct_gep_raw(c, coerce, coerce_type, info->coerce_expand.lo_index,
			                                           return_value->alignment, &alignment);
			LLVMTypeRef lo_type = llvm_abi_type(c, info->coerce_expand.lo);
			lo_val = llvm_load(c, lo_type, lo, alignment, "");

			// We're done if there's a single field.
			if (!abi_type_is_valid(info->coerce_expand.hi))
			{
				llvm_emit_return_value(c, lo_val);
				return;
			}

			// Let's make a first class aggregate
			LLVMValueRef hi = llvm_emit_struct_gep_raw(c, coerce, coerce_type, info->coerce_expand.hi_index,
			                                           return_value->alignment, &alignment);
			LLVMTypeRef hi_type = llvm_abi_type(c, info->coerce_expand.hi);
			LLVMValueRef hi_val = llvm_load(c, hi_type, hi, alignment, "");

			LLVMTypeRef unpadded_type = llvm_get_twostruct(c, lo_type, hi_type);
			LLVMValueRef composite = llvm_get_undef_raw(unpadded_type);

			composite = llvm_emit_insert_value(c, composite, lo_val, 0);
			composite = llvm_emit_insert_value(c, composite, hi_val, 1);

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
	llvm_value_set(&value, llvm_get_zero(c, type_anyerr), type_anyerr);
	llvm_emit_return_abi(c, NULL, &value);
}

void llvm_emit_function_body(GenContext *c, Decl *decl)
{
	DEBUG_LOG("Generating function %s.", decl->extname);
	assert(decl->backend_ref);
	llvm_emit_body(c,
	               decl->backend_ref,
	               decl->unit->module->name->module,
	               decl->name,
	               decl->span.file_id,
	               decl->type->function.prototype,
	               decl->func_decl.attr_naked ? NULL : &decl->func_decl.signature,
	               astptr(decl->func_decl.body));
}

void llvm_emit_body(GenContext *c, LLVMValueRef function, const char *module_name, const char *function_name,
                    FileId file_id, FunctionPrototype *prototype, Signature *signature, Ast *body)
{

	bool emit_debug = llvm_use_debug(c);
	LLVMValueRef prev_function = c->function;
	LLVMBuilderRef prev_builder = c->builder;

	c->opt_var = NULL;
	c->catch_block = NULL;

	c->function = function;
	if (!function_name) function_name = "anonymous function";
	if (emit_debug)
	{
		c->debug.function = LLVMGetSubprogram(function);
		if (c->debug.enable_stacktrace)
		{
			scratch_buffer_clear();
			scratch_buffer_append(module_name);
			scratch_buffer_append("::");
			scratch_buffer_append(function_name);
			c->debug.func_name = llvm_emit_string_const(c, scratch_buffer_to_string(), ".funcname");

			File *file = source_file_by_id(file_id);
			c->debug.file_name = llvm_emit_string_const(c, file->name, ".filename");
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
		if (c->debug.enable_stacktrace)
		{
			LLVMTypeRef slot_type = c->debug.stack_type;
			LLVMTypeRef ptr_to_slot_type = LLVMPointerType(slot_type, 0);
			if (!c->debug.last_ptr)
			{
				const char *name = ".$last_stack";
				LLVMValueRef last_stack = c->debug.last_ptr = llvm_add_global_raw(c, name, ptr_to_slot_type, 0);
				LLVMSetThreadLocal(last_stack, true);
				LLVMSetInitializer(last_stack, llvm_get_zero_raw(ptr_to_slot_type));
				llvm_set_weak(c, last_stack);
			}
			AlignSize alignment = llvm_abi_alignment(c, slot_type);
			c->debug.stack_slot = llvm_emit_alloca(c, slot_type, alignment, ".$stackslot");
			AlignSize align_to_use;
			LLVMValueRef prev_ptr = llvm_emit_struct_gep_raw(c, c->debug.stack_slot, slot_type, 0, alignment, &align_to_use);
			llvm_store_to_ptr_raw_aligned(c,
			                              prev_ptr,
			                              LLVMBuildLoad2(c->builder, ptr_to_slot_type, c->debug.last_ptr, ""),
			                              align_to_use);
			LLVMValueRef func_name = llvm_emit_struct_gep_raw(c, c->debug.stack_slot, slot_type, 1, alignment, &align_to_use);
			llvm_store_to_ptr_raw_aligned(c, func_name, c->debug.func_name, align_to_use);
			LLVMValueRef file_name = llvm_emit_struct_gep_raw(c, c->debug.stack_slot, slot_type, 2, alignment, &align_to_use);
			llvm_store_to_ptr_raw_aligned(c, file_name, c->debug.file_name, align_to_use);
			c->debug.stack_slot_row = llvm_emit_struct_gep_raw(c, c->debug.stack_slot, slot_type, 3, alignment, &align_to_use);
			LLVMValueRef last_ptr = NULL;
			if (function_name != kw_main && function_name != kw_mainstub)
			{
				last_ptr = c->debug.last_ptr;
			}
			else
			{
				last_ptr = prev_ptr;
			}
			llvm_store_to_ptr_raw_aligned(c,
			                              last_ptr,
			                              c->debug.stack_slot,
			                              type_alloca_alignment(type_voidptr));
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
	}

	c->builder = prev_builder;
	c->function = prev_function;
}


void llvm_emit_xxlizer(GenContext *c, Decl *decl)
{
	Ast *body = astptrzero(decl->xxlizer.init);
	if (!body)
	{
		// Skip if it doesn't have a body.
		return;
	}
	LLVMTypeRef initializer_type = LLVMFunctionType(LLVMVoidTypeInContext(c->context), NULL, 0, false);
	bool is_finalizer = decl->decl_kind == DECL_FINALIZE;
	LLVMValueRef **array_ref = is_finalizer ? &c->destructors : &c->constructors;
	scratch_buffer_clear();
	scratch_buffer_printf(is_finalizer ? ".static_finalize.%u" : ".static_initialize.%u", vec_size(*array_ref));
	LLVMValueRef function = LLVMAddFunction(c->module, scratch_buffer_to_string(), initializer_type);
	LLVMSetLinkage(function, LLVMInternalLinkage);
	if (llvm_use_debug(c))
	{
		uint32_t row = decl->span.row;
		if (!row) row = 1;
		LLVMMetadataRef type = LLVMDIBuilderCreateSubroutineType(c->debug.builder, c->debug.file, NULL, 0, 0);

		c->debug.function = LLVMDIBuilderCreateFunction(c->debug.builder,
		                                                c->debug.file,
		                                                scratch_buffer.str, scratch_buffer.len,
		                                                scratch_buffer.str, scratch_buffer.len,
		                                                c->debug.file,
														row,
		                                                type,
		                                                true,
														true,
		                                                row,
		                                                LLVMDIFlagPrivate,
		                                                active_target.optimization_level != OPTIMIZATION_NONE);
		LLVMSetSubprogram(function, c->debug.function);
	}
	llvm_emit_body(c,
	               function,
	               decl->unit->module->name->module,
	               is_finalizer ? "[static finalizer]" : "[static initializer]",
	               decl->span.file_id,
	               NULL,
	               NULL,
	               body);
	unsigned priority = decl->xxlizer.priority;
	LLVMValueRef vals[3] = { llvm_const_int(c, type_int, priority), function, llvm_get_zero(c, type_voidptr) };
	vec_add(*array_ref, LLVMConstStructInContext(c->context, vals, 3, false));
}

void llvm_emit_function_decl(GenContext *c, Decl *decl)
{
	assert(decl->decl_kind == DECL_FUNC);
	// Resolve function backend type for function.

	LLVMValueRef function = llvm_get_ref(c, decl);
	decl->backend_ref = function;
	if (decl->section)
	{
		LLVMSetSection(function, decl->section);
	}
	if (llvm_use_debug(c))
	{
		llvm_emit_debug_function(c, decl);
	}

	Visibility visibility = decl->visibility;
	if (decl->is_external_visible) visibility = VISIBLE_PUBLIC;
	switch (visibility)
	{
		case VISIBLE_EXTERN:
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
			if (decl->type->function.prototype->call_abi == CALL_X86_STD && platform_target.os == OS_TYPE_WIN32)
			{
				LLVMSetDLLStorageClass(function, LLVMDLLImportStorageClass);
			}
			break;
		case VISIBLE_PUBLIC:
		case VISIBLE_MODULE:
			if (decl->is_weak) llvm_set_weak(c, function);
			break;
		case VISIBLE_LOCAL:
			LLVMSetLinkage(function, decl->is_weak ? LLVMLinkerPrivateWeakLinkage : LLVMInternalLinkage);
			LLVMSetVisibility(function, LLVMDefaultVisibility);
			break;;
	}
}


