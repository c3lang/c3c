// Copyright (c) 2019 Christoffer Lerno. All rights reserved.
// Use of this source code is governed by the GNU LGPLv3.0 license
// a copy of which can be found in the LICENSE file.

#include "llvm_codegen_internal.h"


void gencontext_begin_module(GenContext *c)
{
	assert(!c->module && "Expected no module");

	c->ir_filename = strformat("%.*s.ll", (int)strlen(c->ast_context->file->name) - 3, c->ast_context->file->name);
	c->object_filename = strformat("%.*s.o", (int)strlen(c->ast_context->file->name) - 3, c->ast_context->file->name);

	const char *full_path = c->ast_context->file->full_path;
	char *mangled_module_name = strformat("%s-%s", c->ast_context->module->name->module, c->ast_context->file->name);
	c->module = LLVMModuleCreateWithNameInContext(mangled_module_name, c->context);
	c->machine = llvm_target_machine_create();
	c->target_data = LLVMCreateTargetDataLayout(c->machine);
	LLVMSetModuleDataLayout(c->module, c->target_data);
	LLVMSetSourceFileName(c->module, full_path, strlen(c->ast_context->file->full_path));
	LLVMTypeRef options_type = LLVMInt8TypeInContext(c->context);

	if (active_target.pic == PIC_BIG || active_target.pic == PIC_SMALL)
	{
		static const char *pic_level = "PIC Level";
		LLVMMetadataRef setting = LLVMValueAsMetadata(LLVMConstInt(options_type, active_target.pic, false));
		LLVMAddModuleFlag(c->module, LLVMModuleFlagBehaviorOverride, pic_level, strlen(pic_level), setting);
	}
	if (active_target.pie == PIE_BIG || active_target.pie == PIE_SMALL)
	{
		static const char *pie_level = "PIE Level";
		LLVMMetadataRef setting = LLVMValueAsMetadata(LLVMConstInt(options_type, active_target.pie, false));
		LLVMAddModuleFlag(c->module, LLVMModuleFlagBehaviorOverride, pie_level, strlen(pie_level), setting);
	}

	LLVMSetTarget(c->module, platform_target.target_triple);
	if (active_target.debug_info != DEBUG_INFO_NONE)
	{
		const char *filename = c->ast_context->file->name;
		const char *dir_path = c->ast_context->file->dir_path;
		// Set runtime version here.
		c->debug.runtime_version = 1;
		c->debug.builder = LLVMCreateDIBuilder(c->module);
		c->debug.file = LLVMDIBuilderCreateFile(c->debug.builder, filename, strlen(filename), dir_path, strlen(dir_path));

		bool is_optimized = active_target.optimization_level != OPTIMIZATION_NONE;
		const char *dwarf_flags = "";
		unsigned runtime_version = 1;
		LLVMDWARFEmissionKind emission_kind = active_target.debug_info == DEBUG_INFO_FULL ? LLVMDWARFEmissionFull : LLVMDWARFEmissionLineTablesOnly;
		c->debug.compile_unit = LLVMDIBuilderCreateCompileUnit(c->debug.builder, LLVMDWARFSourceLanguageC,
		                                                       c->debug.file, DWARF_PRODUCER_NAME,
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

	c->block_global_unique_count = 0;
	c->ast_alloca_addr_space = target_alloca_addr_space();

	VECEACH(global_context.type, i)
	{
		global_context.type[i]->backend_type = NULL;
		global_context.type[i]->backend_debug_type = NULL;
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
