#include "tilde_internal.h"



static TB_System os_to_tilde_system(OsType target)
{
	switch (target)
	{
		case OS_TYPE_LINUX:
			return TB_SYSTEM_LINUX;
		case OS_TYPE_MACOSX:
			return TB_SYSTEM_MACOS;
		case OS_TYPE_WIN32:
			return TB_SYSTEM_WINDOWS;
		case OS_TYPE_WASI:
		case OS_TYPE_EMSCRIPTEN:
			return TB_SYSTEM_WEB;
		default:
			error_exit("Unsupported system for TB compilation, use LLVM.");
	}
}
static TB_Arch arch_to_tilde_arch(ArchType target)
{
	switch (target)
	{
		case ARCH_TYPE_AARCH64:
			return TB_ARCH_AARCH64;
		case ARCH_TYPE_X86_64:
			return TB_ARCH_X86_64;
		case ARCH_TYPE_WASM32:
			return TB_ARCH_WASM32;
		default:
			error_exit("Unsupported architecture for TB compilation, use LLVM.");
	}
	UNREACHABLE
}

static void tilde_emit_type_decls(TildeContext *c, Decl *decl)
{
	switch (decl->decl_kind)
	{
		case DECL_POISONED:
			UNREACHABLE;
		case DECL_FUNC:
			// TODO
			break;
		case DECL_VAR:
			// TODO
			break;
		case DECL_TYPEDEF:
			break;
		case DECL_ENUM_CONSTANT:
		case DECL_FAULTVALUE:
			// TODO
			break;;
		case DECL_DISTINCT:
		case DECL_STRUCT:
		case DECL_UNION:
		case DECL_ENUM:
		case DECL_FAULT:
		case DECL_BITSTRUCT:
			tilde_get_typeid(c, decl->type);
			break;
		case DECL_BODYPARAM:
		case NON_TYPE_DECLS:
			UNREACHABLE
	}
}

TB_Reg tilde_get_next_param(TildeContext *c, unsigned *index)
{
	return tb_inst_param(c->f, (*index)++);
}

static inline void tilde_process_parameter_value(TildeContext *c, Decl *decl, ABIArgInfo *info, unsigned *index)
{
	switch (info->kind)
	{
		case ABI_ARG_IGNORE:
			return;
		case ABI_ARG_INDIRECT:
			// Indirect is caller copied.
			decl->tb_register = tilde_get_next_param(c, index);
			return;
			/*
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
		}*/

		case ABI_ARG_DIRECT:
		//DIRECT_FROM_COERCE:
			if (!decl->var.is_written && !decl->var.is_addr)
			{
				decl->tb_register = tilde_get_next_param(c, index);
				decl->is_value = true;
				return;
			}
			tilde_emit_and_set_decl_alloca(c, decl);
			tilde_store_decl_raw(c, decl, tilde_get_next_param(c, index));
			return;
		case ABI_ARG_DIRECT_SPLIT_STRUCT:
		{
			TODO
			/*
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
			return;*/
		}
		case ABI_ARG_DIRECT_COERCE:
		{
			TODO
			/*
			LLVMTypeRef coerce_type = llvm_get_type(c, info->direct_coerce_type);
			if (coerce_type == llvm_get_type(c, decl->type))
			{
				goto DIRECT_FROM_COERCE;
			}
			llvm_emit_and_set_decl_alloca(c, decl);

			LLVMValueRef param = llvm_get_next_param(c, index);
			// Store it with the alignment of the decl.
			llvm_emit_coerce_store(c, decl->backend_ref, decl->alignment, coerce_type, param, llvm_get_type(c, decl->type));
			return;*/
		}
		case ABI_ARG_DIRECT_COERCE_INT:
		{
			TODO
			/* TODO
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
			}*/
		}
		default:
			TODO
	}
}

static inline void tilde_emit_func_parameter(TildeContext *c, Decl *decl, ABIArgInfo *abi_info, unsigned *index, unsigned real_index)
{
	assert(decl->decl_kind == DECL_VAR && decl->var.kind == VARDECL_PARAM);

	// Allocate room on stack, but do not copy.
	tilde_process_parameter_value(c, decl, abi_info, index);
	if (tilde_use_debug(c))
	{
		TODO
		// TODO llvm_emit_debug_parameter(context, decl, real_index);
	}
}

void tilde_emit_body(TildeContext *c, TB_Function *function, const char *module_name, const char *function_name,
                    FileId file_id, FunctionPrototype *prototype, Signature *signature, Ast *body)
{

	bool emit_debug = tilde_use_debug(c);
	TB_Function *prev_function = c->f;

	c->f = function;
	c->opt_var = TB_NULL_REG;
	c->catch_block = 0;

	if (!function_name) function_name = "anonymous function";
	if (emit_debug)
	{
		TODO
		/*
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
		}*/
	}

	c->cur_func.name = function_name;
	c->cur_func.prototype = prototype;


	unsigned arg = 0;

	if (emit_debug)
	{
		TODO /*
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
		}*/
	}

	c->optional_out = TB_NULL_REG;
	c->return_out = TB_NULL_REG;
	if (prototype && prototype->ret_abi_info->kind == ABI_ARG_INDIRECT)
	{
		if (prototype->is_optional)
		{
			c->optional_out = tb_inst_param(c->f, arg++);
		}
		else
		{
			c->return_out = tb_inst_param(c->f, arg++);
		}
	}
	if (prototype && prototype->ret_by_ref_abi_info)
	{
		assert(!c->return_out);
		c->return_out = tb_inst_param(c->f, arg++);
	}


	if (signature)
	{
		// Generate LLVMValueRef's for all parameters, so we can use them as local vars in code
		FOREACH_BEGIN_IDX(i, Decl *param, signature->params)
			tilde_emit_func_parameter(c, param, prototype->abi_args[i], &arg, i);
		FOREACH_END();
	}

	/*-- TODO
	LLVMSetCurrentDebugLocation2(c->builder, NULL);*/

	AstId current = body->compound_stmt.first_stmt;
	while (current)
	{
		tilde_emit_stmt(c, ast_next(&current));
	}

	/*
	 * TODO
	if (c->current_block && llvm_basic_block_is_unused(c->current_block))
	{
		LLVMBasicBlockRef prev_block = LLVMGetPreviousBasicBlock(c->current_block);
		LLVMDeleteBasicBlock(c->current_block);
		c->current_block = prev_block;
		LLVMPositionBuilderAtEnd(c->builder, c->current_block);
	}*/

	// Insert a return (and defer) if needed.
	if (!tb_basic_block_is_complete(c->f, tb_inst_get_label(c->f)))
	{
		tilde_emit_return_implicit(c);
	}
	if (tilde_use_debug(c))
	{
		TODO
		//llvm_debug_scope_pop(c);
	}

	c->f = prev_function;
}

static void tilde_emit_function_body(TildeContext *c, Decl *decl)
{
	TB_Function *fn = decl->tb_symbol;
	vec_add(c->functions, fn);
	DEBUG_LOG("Generating function %s.", decl->extname);
	assert(decl->backend_ref);
	tilde_emit_body(c,
	               fn,
	               decl->unit->module->name->module,
	               decl->name,
	               decl->span.file_id,
	               decl->type->function.prototype,
	               decl->func_decl.attr_naked ? NULL : &decl->func_decl.signature,
	               astptr(decl->func_decl.body));
}

static TildeContext *tilde_gen_module(Module *module, TB_FeatureSet *feature_set)
{
	if (!vec_size(module->units)) return NULL;
	TildeContext *context = CALLOCS(TildeContext);
	TB_Module *tb_module = tb_module_create(arch_to_tilde_arch(platform_target.arch),
	                                        os_to_tilde_system(platform_target.os),
	                                        feature_set, false);
	codegen_setup_object_names(module, &context->ir_filename, &context->asm_filename, &context->object_filename);
	context->module = tb_module;

	FOREACH_BEGIN(CompilationUnit *unit, module->units)

		REMINDER("Add debug info");
		/* gencontext_init_file_emit(gen_context, unit);
		context->debug.compile_unit = unit->llvm.debug_compile_unit;
		gen_context->debug.file = unit->llvm.debug_file;*/

		FOREACH_BEGIN(Decl *initializer, unit->xxlizers)
			REMINDER("Add xxlizer");
			//tilde_emit_xxlizer(gen_context, initializer);
		FOREACH_END();

		FOREACH_BEGIN(Decl *method, unit->methods)
			tilde_emit_function_decl(context, method);
		FOREACH_END();

		FOREACH_BEGIN(Decl *type_decl, unit->types)
			tilde_emit_type_decls(context, type_decl);
		FOREACH_END();

		FOREACH_BEGIN(Decl *enum_decl, unit->enums)
			tilde_emit_type_decls(context, enum_decl);
		FOREACH_END();

		FOREACH_BEGIN(Decl *func, unit->functions)
			if (func->func_decl.attr_test)
			{
				if (!active_target.testing) continue;
				vec_add(module->tests, func);
			}
			if (func->func_decl.attr_benchmark)
			{
				if (!active_target.benchmarking) continue;
				vec_add(module->benchmarks, func);
			}
			tilde_emit_function_decl(context, func);
		FOREACH_END();

		if (active_target.type != TARGET_TYPE_TEST && active_target.type != TARGET_TYPE_BENCHMARK && unit->main_function && unit->main_function->is_synthetic)
		{
			tilde_emit_function_decl(context, unit->main_function);
		}

	FOREACH_END();

	FOREACH_BEGIN(CompilationUnit *unit, module->units)

	/*--- TODO
		context->debug.compile_unit = unit->llvm.debug_compile_unit;
		context->debug.file = unit->llvm.debug_file;
*/
		FOREACH_BEGIN(Decl *var, unit->vars)
			tilde_get_ref(context, var);
		FOREACH_END();

		FOREACH_BEGIN(Decl *var, unit->vars)
			// TODO tilde_emit_global_variable_init(context, var);
		FOREACH_END();

		FOREACH_BEGIN(Decl *decl, unit->functions)
			if (decl->func_decl.attr_test && !active_target.testing) continue;
			if (decl->func_decl.body) tilde_emit_function_body(context, decl);
		FOREACH_END();

		if (active_target.type != TARGET_TYPE_TEST && active_target.type != TARGET_TYPE_BENCHMARK && unit->main_function && unit->main_function->is_synthetic)
		{
			tilde_emit_function_body(context, unit->main_function);
		}

		FOREACH_BEGIN(Decl *decl, unit->methods)
			if (decl->func_decl.body) tilde_emit_function_body(context, decl);
		FOREACH_END();

		// gencontext_end_file_emit(gen_context, unit);

	FOREACH_END();

	/*-- TODO
	tilde_emit_constructors_and_destructors(context); */

	// EmitDeferred()

	/*-- TODO
	if (llvm_use_debug(gen_context))
	{
		LLVMDIBuilderFinalize(gen_context->debug.builder);
		LLVMDisposeDIBuilder(gen_context->debug.builder);
	}*/

	// If it's in test, then we want to serialize the IR before it is optimized.
	/*--if (active_target.test_output)
	{
		gencontext_print_llvm_ir(gen_context);
		gencontext_verify_ir(gen_context);
	}--*/
	return context;
}

void **tilde_gen(Module **modules, unsigned module_count)
{
	if (!module_count) return NULL;
	TB_FeatureSet feature_set;
	switch (platform_target.arch)
	{
		case ARCH_TYPE_AARCH64:
			feature_set.aarch64.bf16 = false;
		case ARCH_TYPE_X86_64:
			feature_set.x64.avx = platform_target.x64.x86_vector_capability >= X86VECTOR_AVX;
		default:
			break;
	}
	TildeContext **contexts = NULL;
	for (unsigned i = 0; i < module_count; i++)
	{
		TildeContext *c = tilde_gen_module(modules[i], &feature_set);
		if (c) vec_add(contexts, c);
	}
	return (void**)contexts;
}

void tinybackend_codegen_setup()
{
}


static TB_DataType tilde_get_abi_type(AbiType type)
{
	if (abi_type_is_type(type)) return tildetype(type.type);
	TODO
}

TB_DataType tilde_get_int_type_of_bytesize(int byte_size)
{
	switch (byte_size)
	{
		case 1:
			return TB_TYPE_I8;
		case 2:
			return TB_TYPE_I16;
		case 3:
		case 4:
			return TB_TYPE_I32;
		case 5:
		case 6:
		case 7:
		case 8:
			return TB_TYPE_I64;
		case 16:
			return (TB_DataType) { .type = TB_INT, .width = 0, .data = 128 };
		default:
			FATAL_ERROR("Unsupported size");
	}

}
static void param_expand(TB_DataType **params_ref, Type *type)
{
	switch (type->type_kind)
	{
		case TYPE_TYPEDEF:
			UNREACHABLE
		case TYPE_ARRAY:
			for (ArraySize i = type->array.len; i > 0; i--)
			{
				param_expand(params_ref, type->array.base);
			}
			return;
		case TYPE_STRUCT:
		{
			Decl **members = type->decl->strukt.members;
			VECEACH(members, i)
			{
				param_expand(params_ref, members[i]->type);
			}
			return;
		}
		case TYPE_ENUM:
		case TYPE_ANYFAULT:
		case TYPE_FAULTTYPE:
			param_expand(params_ref, type_lowering(type));
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
			param_expand(params_ref, largest_type);
			return;
		}
		default:
			vec_add(*params_ref, tildetype(type));
			return;
	}

}

static void callback(void* user_data, const char* fmt, ...)
{
	va_list list;
	va_start(list, fmt);
	vprintf(fmt, list);
	va_end(list);
}

// Compile module (multi threaded)
const char *tilde_codegen(void *context)
{
	TildeContext *c = (TildeContext *)context;
	bool is_win32 = platform_target.os == OS_TYPE_WIN32;
	TB_DebugFormat debug_format = is_win32 ? TB_DEBUGFMT_CODEVIEW : TB_DEBUGFMT_DWARF;
	if (active_target.debug_info == DEBUG_INFO_NONE) debug_format = TB_DEBUGFMT_NONE;

	FOREACH_BEGIN(TB_Function *function, c->functions)
		if (!tb_module_compile_function(c->module, function, TB_ISEL_FAST))
		{
			error_exit("Failed to compile function.");
		}
		tb_function_print(function, &callback, NULL, true);
	FOREACH_END();

	const char *object_name = NULL;
	if (active_target.emit_object_files)
	{
		if (!tb_exporter_write_files(c->module, TB_FLAVOR_OBJECT, debug_format, 1, &c->object_filename))
		{
			error_exit("Failed to create object file %s.", c->object_filename);
		}
		object_name = c->object_filename;
	}

	if (active_target.emit_asm && false)
	{
		if (!tb_exporter_write_files(c->module, TB_FLAVOR_ASSEMBLY, debug_format, 1, &c->asm_filename))
		{
			error_exit("Failed to create asm %s.", c->asm_filename);
		}
	}

	tb_module_destroy(c->module);
	return object_name;


	return c->object_filename;
}

