// Copyright (c) 2019 Christoffer Lerno. All rights reserved.
// Use of this source code is governed by the GNU LGPLv3.0 license
// a copy of which can be found in the LICENSE file.

#include "llvm_codegen_internal.h"


void gencontext_begin_module(GenContext *context)
{
	assert(!context->module && "Expected no module");
	const char *full_path = context->ast_context->file->full_path;
	char *mangled_module_name = strformat("%s-%s", context->ast_context->module->name->module, context->ast_context->file->name);
	context->module = LLVMModuleCreateWithNameInContext(mangled_module_name, context->context);
	LLVMSetModuleDataLayout(context->module, target_data_layout());
	LLVMSetSourceFileName(context->module, full_path, strlen(context->ast_context->file->full_path));

	LLVMSetTarget(context->module, build_target.target_triple);
	if (build_options.debug_info != DEBUG_INFO_NONE)
	{
		const char *filename = context->ast_context->file->name;
		const char *dir_path = context->ast_context->file->dir_path;
		// Set runtime version here.
		context->debug.runtime_version = 1;
		context->debug.builder = LLVMCreateDIBuilder(context->module);
		context->debug.file = LLVMDIBuilderCreateFile(context->debug.builder, filename, strlen(filename), dir_path, strlen(dir_path));

		bool is_optimized = build_options.optimization_level != OPTIMIZATION_NONE;
		const char *dwarf_flags = "";
		unsigned runtime_version = 1;
		LLVMDWARFEmissionKind emission_kind = build_options.debug_info == DEBUG_INFO_FULL ? LLVMDWARFEmissionFull : LLVMDWARFEmissionLineTablesOnly;
		context->debug.compile_unit = LLVMDIBuilderCreateCompileUnit(context->debug.builder, LLVMDWARFSourceLanguageC,
		                                                             context->debug.file, DWARF_PRODUCER_NAME,
		                                                             strlen(DWARF_PRODUCER_NAME), is_optimized,
		                                                             dwarf_flags, strlen(dwarf_flags),
		                                                             runtime_version, "" /* split name */, 0 /* len */,
		                                                             emission_kind, /* dwo */0, /* inlining */0,
		                                                             /* debug for profiling */0
#if LLVM_VERSION_MAJOR > 10
		                                                             , "", 0, "", 0
#endif
		                                                             );
	}
	// Setup all types. Not thread-safe, but at this point in time we can assume a single context.
	// We need to remove the context from the cache after this.
	// This would seem to indicate that we should change Type / actual type.

	context->block_global_unique_count = 0;
	context->ast_alloca_addr_space = target_alloca_addr_space();

	VECEACH(compiler.type, i)
	{
		compiler.type[i]->backend_type = NULL;
	}
}


void gencontext_end_module(GenContext *context)
{
	if (llvm_use_debug(context))
	{
		LLVMDIBuilderFinalize(context->debug.builder);
	}
	LLVMDisposeModule(context->module);
}
