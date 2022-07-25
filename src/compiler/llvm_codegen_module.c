// Copyright (c) 2019 Christoffer Lerno. All rights reserved.
// Use of this source code is governed by the GNU LGPLv3.0 license
// a copy of which can be found in the LICENSE file.

#include "llvm_codegen_internal.h"



static inline LLVMTypeRef create_introspection_type(GenContext *c)
{
	LLVMTypeRef type = LLVMStructCreateNamed(c->context, ".introspect");
	LLVMTypeRef typeid_type = llvm_get_type(c, type_typeid);
	LLVMTypeRef kind_type = llvm_get_type(c, type_char);
	LLVMTypeRef usize_type = llvm_get_type(c, type_usize);
	LLVMTypeRef introspect_type[INTROSPECT_INDEX_TOTAL] = {
			[INTROSPECT_INDEX_KIND] = kind_type,
			[INTROSPECT_INDEX_SIZEOF] = usize_type,
			[INTROSPECT_INDEX_INNER] = typeid_type,
			[INTROSPECT_INDEX_LEN] = usize_type,
			[INTROSPECT_INDEX_ADDITIONAL] = LLVMArrayType(typeid_type, 0),
	};
	LLVMStructSetBody(type, introspect_type, INTROSPECT_INDEX_TOTAL, false);
	return type;
}

static inline LLVMTypeRef create_fault_type(GenContext *c)
{
	LLVMTypeRef type = LLVMStructCreateNamed(c->context, ".fault");
	LLVMTypeRef typeid_type = llvm_get_type(c, type_typeid);
	LLVMTypeRef chars_type = llvm_get_type(c, type_chars);
	LLVMTypeRef fault_type[] = {
			[0] = typeid_type,
			[1] = chars_type,
	};
	LLVMStructSetBody(type, fault_type, 2, false);
	return type;
}

void gencontext_begin_module(GenContext *c)
{
	assert(!c->module && "Expected no module");

	const char *result = module_create_object_file_name(c->code_module);
	c->ir_filename = str_printf("%s.ll", result);
	if (active_target.llvm_file_dir) c->ir_filename = file_append_path(active_target.llvm_file_dir, c->ir_filename);
	c->object_filename = str_printf("%s%s", result, get_object_extension());
	if (active_target.object_file_dir) c->object_filename = file_append_path(active_target.object_file_dir, c->object_filename);
	c->panicfn = global_context.panic_fn;
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
				// TODO
			default:
				break;
		}
	}
	c->bool_type = LLVMInt1TypeInContext(c->context);
	c->byte_type = LLVMInt8TypeInContext(c->context);
	c->introspect_type = create_introspection_type(c);
	c->fault_type = create_fault_type(c);
	c->size_type = llvm_get_type(c, type_usize);
	if (c->panicfn) c->panicfn->backend_ref = NULL;

	if (active_target.debug_info != DEBUG_INFO_NONE)
	{
		c->debug.runtime_version = 1;
		c->debug.builder = LLVMCreateDIBuilder(c->module);
		if (active_target.debug_info == DEBUG_INFO_FULL && active_target.feature.safe_mode)
		{
			c->debug.stack_type = LLVMStructCreateNamed(c->context, ".$callstack");
			LLVMTypeRef types[4] = { LLVMPointerType(c->debug.stack_type, 0),
									 LLVMPointerType(c->byte_type, 0),
									 LLVMPointerType(c->byte_type, 0),
									 llvm_get_type(c, type_uint) };
			LLVMStructSetBody(c->debug.stack_type, types, 4, false);
			c->debug.last_ptr = NULL;
			c->debug.enable_stacktrace = true;
		}
	}
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
