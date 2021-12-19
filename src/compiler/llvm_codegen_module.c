// Copyright (c) 2019 Christoffer Lerno. All rights reserved.
// Use of this source code is governed by the GNU LGPLv3.0 license
// a copy of which can be found in the LICENSE file.

#include "llvm_codegen_internal.h"

const char *get_object_extension(void)
{
	switch (active_target.arch_os_target)
	{
		case X64_WINDOWS:
		case X86_WINDOWS:
		case X64_WINDOWS_GNU:
			return ".obj";
		default:
			return ".o";
	}
}

void gencontext_begin_module(GenContext *c)
{
	assert(!c->module && "Expected no module");

	scratch_buffer_clear();
	StringSlice slice = strtoslice(c->code_module->name->module);
	while (true)
	{
		StringSlice part = strnexttok(&slice, ':');
		scratch_buffer_append_len(part.ptr, part.len);
		if (!slice.len) break;
		slice.ptr++;
		slice.len--;
		scratch_buffer_append_char('.');
	}
	const char *result = scratch_buffer_to_string();
	c->ir_filename = strformat("%s.ll", result);
	c->object_filename = strformat("%s%s", result, get_object_extension());

	c->module = LLVMModuleCreateWithNameInContext(c->code_module->name->module, c->context);
	c->machine = llvm_target_machine_create();
	c->target_data = LLVMCreateTargetDataLayout(c->machine);
	LLVMSetModuleDataLayout(c->module, c->target_data);
	LLVMSetSourceFileName(c->module, c->code_module->name->module, strlen(c->code_module->name->module));
	LLVMTypeRef options_type = LLVMInt8TypeInContext(c->context);

	if (active_target.pic == PIC_BIG || active_target.pic == PIC_SMALL)
	{
		static const char *pic_level = "PIC Level";
		LLVMMetadataRef setting = LLVMValueAsMetadata(LLVMConstInt(options_type, (unsigned)active_target.pic, false));
		LLVMAddModuleFlag(c->module, LLVMModuleFlagBehaviorOverride, pic_level, strlen(pic_level), setting);
	}
	if (active_target.pie == PIE_BIG || active_target.pie == PIE_SMALL)
	{
		static const char *pie_level = "PIE Level";
		LLVMMetadataRef setting = LLVMValueAsMetadata(LLVMConstInt(options_type, (unsigned)active_target.pie, false));
		LLVMAddModuleFlag(c->module, LLVMModuleFlagBehaviorOverride, pie_level, strlen(pie_level), setting);
	}

	LLVMSetTarget(c->module, platform_target.target_triple);

	// Setup all types. Not thread-safe, but at this point in time we can assume a single context.
	// We need to remove the context from the cache after this.
	// This would seem to indicate that we should change Type / actual type.

	c->block_global_unique_count = 0;
	c->ast_alloca_addr_space = target_alloca_addr_space();

	VECEACH(global_context.type, i)
	{
		global_context.type[i]->backend_type = NULL;
		global_context.type[i]->backend_debug_type = NULL;
		global_context.type[i]->backend_typeid = NULL;
	}
	if (active_target.debug_info != DEBUG_INFO_NONE)
	{
		c->debug.runtime_version = 1;
		c->debug.builder = LLVMCreateDIBuilder(c->module);
	}
}

void gencontext_init_file_emit(GenContext *c, Context *ast)
{
	if (active_target.debug_info != DEBUG_INFO_NONE)
	{
		const char *filename = ast->file->name;
		const char *dir_path = ast->file->dir_path;
		// Set runtime version here.
		ast->llvm_debug_file = LLVMDIBuilderCreateFile(c->debug.builder,
		                                               filename,
		                                               strlen(filename),
		                                               dir_path,
		                                               strlen(dir_path));

		bool is_optimized = active_target.optimization_level != OPTIMIZATION_NONE;
		const char *dwarf_flags = "";
		unsigned runtime_version = 1;
		LLVMDWARFEmissionKind emission_kind =
				active_target.debug_info == DEBUG_INFO_FULL ? LLVMDWARFEmissionFull : LLVMDWARFEmissionLineTablesOnly;
		const char *debug_output_file = "";
		bool emit_debug_info_for_profiling = false;
		bool split_debug_inlining = false;
		const char *sysroot = "";
		const char *sdk = "";
		unsigned dwo_id = 0;
		ast->llvm_debug_compile_unit = LLVMDIBuilderCreateCompileUnit(c->debug.builder,
		                                                              LLVMDWARFSourceLanguageC,
		                                                              ast->llvm_debug_file,
		                                                              DWARF_PRODUCER_NAME,
		                                                              strlen(DWARF_PRODUCER_NAME),
		                                                              is_optimized,
		                                                              dwarf_flags,
		                                                              strlen(dwarf_flags),
		                                                              runtime_version,
		                                                              debug_output_file,
		                                                              strlen(debug_output_file),
		                                                              emission_kind,
		                                                              dwo_id,
		                                                              split_debug_inlining,
		                                                              emit_debug_info_for_profiling
#if LLVM_VERSION_MAJOR >= 11
		                                                              ,
		                                                              sysroot,
		                                                              strlen(sysroot),
		                                                              sdk,
		                                                              strlen(sdk)
#endif
		                                                      );
	}
}


void gencontext_end_file_emit(GenContext *c, Context *ast)
{
}

void gencontext_end_module(GenContext *context)
{
	if (llvm_use_debug(context))
	{
		LLVMDIBuilderFinalize(context->debug.builder);
	}
	LLVMDisposeModule(context->module);
}
