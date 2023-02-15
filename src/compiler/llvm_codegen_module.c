// Copyright (c) 2019 Christoffer Lerno. All rights reserved.
// Use of this source code is governed by the GNU LGPLv3.0 license
// a copy of which can be found in the LICENSE file.

#include "llvm_codegen_internal.h"



static inline LLVMTypeRef create_introspection_type(GenContext *c)
{
	LLVMTypeRef type = LLVMStructCreateNamed(c->context, ".introspect");
	LLVMTypeRef introspect_type[INTROSPECT_INDEX_TOTAL] = {
			[INTROSPECT_INDEX_KIND] = c->byte_type,
			[INTROSPECT_INDEX_SIZEOF] = c->size_type,
			[INTROSPECT_INDEX_INNER] = c->typeid_type,
			[INTROSPECT_INDEX_LEN] = c->size_type,
			[INTROSPECT_INDEX_ADDITIONAL] = LLVMArrayType(c->typeid_type, 0),
	};
	LLVMStructSetBody(type, introspect_type, INTROSPECT_INDEX_TOTAL, false);
	return type;
}

static inline LLVMTypeRef create_fault_type(GenContext *c)
{
	LLVMTypeRef type = LLVMStructCreateNamed(c->context, ".fault");
	LLVMTypeRef fault_type[] = { c->typeid_type, c->chars_type };
	LLVMStructSetBody(type, fault_type, 2, false);
	return type;
}

void gencontext_begin_module(GenContext *c)
{
	assert(!c->module && "Expected no module");

	codegen_setup_object_names(c->code_module, &c->ir_filename, &c->asm_filename, &c->object_filename);

	c->panic_var = global_context.panic_var;
	c->module = LLVMModuleCreateWithNameInContext(c->code_module->name->module, c->context);
	c->machine = llvm_target_machine_create();
	c->target_data = LLVMCreateTargetDataLayout(c->machine);

	LLVMSetModuleDataLayout(c->module, c->target_data);
	LLVMSetSourceFileName(c->module, c->code_module->name->module, strlen(c->code_module->name->module));
	LLVMTypeRef options_type = LLVMInt8TypeInContext(c->context);

	static const char *pic_level = "PIC Level";
	static const char *pie_level = "PIE Level";
	LLVMMetadataRef setting;
	switch (active_target.reloc_model)
	{
		case RELOC_BIG_PIE:
			setting = LLVMValueAsMetadata(LLVMConstInt(options_type, (unsigned)2 /* PIE */, false));
			LLVMAddModuleFlag(c->module, LLVMModuleFlagBehaviorOverride, pie_level, strlen(pie_level), setting);
			FALLTHROUGH;
		case RELOC_BIG_PIC:
			setting = LLVMValueAsMetadata(LLVMConstInt(options_type, (unsigned)2 /* PIC */, false));
			LLVMAddModuleFlag(c->module, LLVMModuleFlagBehaviorOverride, pic_level, strlen(pic_level), setting);
			break;
		case RELOC_SMALL_PIE:
			setting = LLVMValueAsMetadata(LLVMConstInt(options_type, (unsigned)1 /* pie */, false));
			LLVMAddModuleFlag(c->module, LLVMModuleFlagBehaviorOverride, pie_level, strlen(pie_level), setting);
			FALLTHROUGH;
		case RELOC_SMALL_PIC:
			setting = LLVMValueAsMetadata(LLVMConstInt(options_type, (unsigned)1 /* pic */, false));
			LLVMAddModuleFlag(c->module, LLVMModuleFlagBehaviorOverride, pic_level, strlen(pic_level), setting);
			break;
		default:
			break;
	}

	LLVMSetTarget(c->module, platform_target.target_triple);

	// Setup all types. Not thread-safe, but at this point in time we can assume a single context.
	// We need to remove the context from the cache after this.
	// This would seem to indicate that we should change Type / actual type.

	c->block_global_unique_count = 0;
	c->ast_alloca_addr_space = target_alloca_addr_space();
	VECEACH(global_context.type, i)
	{
		Type *type = global_context.type[i];
		type->backend_type = NULL;
		type->backend_debug_type = NULL;
		type->backend_typeid = NULL;
		switch (type->type_kind)
		{
			case TYPE_ENUM:
			case TYPE_FAULTTYPE:
			{
				Decl **values = type->decl->enums.values;
				VECEACH(values, j)
				{
					values[j]->backend_ref = NULL;
				}
				FALLTHROUGH;
			}
			case TYPE_STRUCT:
			case TYPE_UNION:
			case TYPE_DISTINCT:
				type->decl->backend_ref = NULL;
				break;
			case TYPE_FUNC:
				REMINDER("Clear func when it has reflection");
				break;
			default:
				break;
		}
	}
	c->bool_type = LLVMInt1TypeInContext(c->context);
	c->byte_type = LLVMInt8TypeInContext(c->context);
	c->ptr_type = LLVMPointerType(c->byte_type, 0);
	c->size_type = llvm_get_type(c, type_usz);
	c->typeid_type = llvm_get_type(c, type_typeid);
	c->chars_type = llvm_get_type(c, type_chars);
	c->introspect_type = create_introspection_type(c);
	c->fault_type = create_fault_type(c);
	if (c->panic_var) c->panic_var->backend_ref = NULL;

	if (active_target.debug_info != DEBUG_INFO_NONE)
	{
		if (active_target.arch_os_target == WINDOWS_X64 || active_target.arch_os_target == WINDOWS_AARCH64)
		{
			setting = LLVMValueAsMetadata(LLVMConstInt(options_type, (unsigned)1 /* pic */, false));
			LLVMAddModuleFlag(c->module, LLVMModuleFlagBehaviorError, "CodeView", strlen("CodeView"), setting);
		}
		c->debug.runtime_version = 1;
		c->debug.builder = LLVMCreateDIBuilder(c->module);
		if (active_target.debug_info == DEBUG_INFO_FULL && active_target.feature.safe_mode)
		{
			c->debug.stack_type = LLVMStructCreateNamed(c->context, ".$callstack");
			LLVMTypeRef types[4] = { c->ptr_type,
									 c->chars_type,
									 c->chars_type,
									 llvm_get_type(c, type_uint) };
			LLVMStructSetBody(c->debug.stack_type, types, 4, false);
			c->debug.last_ptr = NULL;
			c->debug.enable_stacktrace = true;
		}
	}
	c->global_builder = LLVMCreateBuilder();
	c->builder = c->global_builder;
}

void gencontext_init_file_emit(GenContext *c, CompilationUnit *unit)
{
	if (active_target.debug_info != DEBUG_INFO_NONE)
	{
		const char *filename = unit->file->name;
		const char *dir_path = unit->file->dir_path;
		// Set runtime version here.
		unit->llvm.debug_file = LLVMDIBuilderCreateFile(c->debug.builder,
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
		if (c->debug.compile_unit)
		{
			unit->llvm.debug_compile_unit = c->debug.compile_unit;
			return;
		}
		unit->llvm.debug_compile_unit = LLVMDIBuilderCreateCompileUnit(c->debug.builder,
		                                                               LLVMDWARFSourceLanguageC,
		                                                               unit->llvm.debug_file,
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
		                                                               emit_debug_info_for_profiling,
		                                                               sysroot,
		                                                               strlen(sysroot),
		                                                               sdk,
		                                                               strlen(sdk)
		                                                              );

	}
}


void gencontext_end_file_emit(GenContext *c, CompilationUnit *ast)
{
}

void gencontext_end_module(GenContext *context)
{
	LLVMDisposeModule(context->module);
}
