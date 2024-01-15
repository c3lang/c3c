// Copyright (c) 2019 Christoffer Lerno. All rights reserved.
// Use of this source code is governed by the GNU LGPLv3.0 license
// a copy of which can be found in the LICENSE file.

#include "llvm_codegen_internal.h"
#include "compiler_tests/benchmark.h"

#include <llvm-c/Error.h>
#include <llvm-c/Comdat.h>
#include <llvm-c/Linker.h>

const char *varargslots_name = "varargslots";
const char *temp_name = "$$temp";
typedef struct LLVMOpaquePassBuilderOptions *LLVMPassBuilderOptionsRef;
LLVMErrorRef LLVMRunPasses(LLVMModuleRef M, const char *Passes,
						   LLVMTargetMachineRef TM,
						   LLVMPassBuilderOptionsRef Options);
LLVMPassBuilderOptionsRef LLVMCreatePassBuilderOptions(void);
void LLVMPassBuilderOptionsSetVerifyEach(LLVMPassBuilderOptionsRef Options, LLVMBool VerifyEach);
void LLVMPassBuilderOptionsSetDebugLogging(LLVMPassBuilderOptionsRef Options, LLVMBool DebugLogging);
void LLVMDisposePassBuilderOptions(LLVMPassBuilderOptionsRef Options);

static void llvm_emit_constructors_and_destructors(GenContext *c);
static void llvm_codegen_setup();
static GenContext *llvm_gen_module(Module *module, LLVMContextRef shared_context);

const char* llvm_version = LLVM_VERSION_STRING;
const char* llvm_target = LLVM_DEFAULT_TARGET_TRIPLE;

static void diagnostics_handler(LLVMDiagnosticInfoRef ref, void *context)
{
	char *message = LLVMGetDiagInfoDescription(ref);
	LLVMDiagnosticSeverity severity = LLVMGetDiagInfoSeverity(ref);
	const char *severity_name;
	switch (severity)
	{
		case LLVMDSError:
			error_exit("LLVM error generating code for %s: %s", ((GenContext *)context)->code_module->name, message);
		case LLVMDSWarning:
			severity_name = "warning";
			break;
		case LLVMDSRemark:
			severity_name = "remark";
			break;
		case LLVMDSNote:
			severity_name = "note";
			break;
		default:
			severity_name = "message";
			break;
	}
#ifdef NDEBUG
	// Avoid warnings when not in debug.
	(void)severity_name; (void)message;
#endif
	DEBUG_LOG("LLVM %s: %s ", severity_name, message);
	LLVMDisposeMessage(message);
}

static void gencontext_init(GenContext *context, Module *module, LLVMContextRef shared_context)
{
	assert(LLVMIsMultithreaded());
	memset(context, 0, sizeof(GenContext));
	if (shared_context)
	{
		context->shared_context = true;
		context->context = shared_context;
	}
	else
	{
		context->context = LLVMContextCreate();
	}
	if (debug_log)
	{
		LLVMContextSetDiagnosticHandler(context->context, &diagnostics_handler, context);
	}
	if (!active_target.emit_llvm && !active_target.test_output && !active_target.benchmark_output ) LLVMContextSetDiscardValueNames(context->context, true);
	context->code_module = module;
}

static void gencontext_destroy(GenContext *context)
{
	assert(llvm_is_global_eval(context));
	LLVMDisposeBuilder(context->global_builder);
	if (!context->shared_context) LLVMContextDispose(context->context);
	LLVMDisposeTargetData(context->target_data);
	LLVMDisposeTargetMachine(context->machine);
	free(context);
}

LLVMBuilderRef llvm_create_builder(GenContext *c)
{
	LLVMBuilderRef builder = LLVMCreateBuilderInContext(c->context);
	LLVMBuilderSetFastMathFlags(builder, active_target.feature.fp_math);
	return builder;
}

LLVMValueRef llvm_emit_is_no_opt(GenContext *c, LLVMValueRef error_value)
{
	LLVMValueRef compare = LLVMBuildICmp(c->builder, LLVMIntEQ, error_value, llvm_get_zero(c, type_anyfault), "not_err");
	return llvm_emit_expect_raw(c, compare);
}

LLVMValueRef llvm_emit_memclear_size_align(GenContext *c, LLVMValueRef ptr, uint64_t size, AlignSize align)
{
	return LLVMBuildMemSet(c->builder, ptr, llvm_get_zero(c, type_char), llvm_const_int(c, type_usz, size), align);
}

INLINE void llvm_emit_xtor(GenContext *c, LLVMValueRef *list, const char *name)
{
	if (!list) return;
	unsigned len = vec_size(list);
	LLVMTypeRef type = LLVMTypeOf(list[0]);
	LLVMValueRef array = LLVMConstArray(type, list, len);
	LLVMValueRef global = LLVMAddGlobal(c->module, LLVMTypeOf(array), name);
	LLVMSetLinkage(global, LLVMAppendingLinkage);
	LLVMSetInitializer(global, array);
}

LLVMValueRef llvm_get_selector(GenContext *c, const char *name)
{
	scratch_buffer_clear();
	scratch_buffer_printf("$sel.%s", name);
	const char *sel_name = scratch_buffer_to_string();
	LLVMValueRef selector_old = LLVMGetNamedGlobal(c->module, sel_name);
	if (selector_old) return selector_old;
	size_t name_len = strlen(name);
	LLVMTypeRef char_array_type = LLVMArrayType(c->byte_type, name_len + 1);
	LLVMValueRef selector = llvm_add_global_raw(c, sel_name, char_array_type, 0);
	LLVMSetGlobalConstant(selector, 1);
	LLVMSetInitializer(selector, llvm_get_zstring(c, name, name_len));
	LLVMSetLinkage(selector, LLVMLinkOnceODRLinkage);
	llvm_set_comdat(c, selector);
	return selector;
}


void llvm_emit_macho_xtor(GenContext *c, LLVMValueRef *list, const char *name)
{
	unsigned len = vec_size(list);
	if (!len) return;
	scratch_buffer_clear();
	scratch_buffer_append(".list$");
	scratch_buffer_append(name);
	LLVMValueRef array = LLVMConstArray(c->xtor_entry_type, list, vec_size(list));
	LLVMValueRef global = LLVMAddGlobal(c->module, LLVMTypeOf(array), scratch_buffer_to_string());
	scratch_buffer_clear();
	scratch_buffer_append("__DATA,__");
	scratch_buffer_append(name);
	LLVMSetLinkage(global, LLVMInternalLinkage);
	LLVMSetInitializer(global, array);
	LLVMSetSection(global, scratch_buffer_to_string());
	LLVMSetAlignment(global, llvm_abi_alignment(c, c->xtor_entry_type));
}

void llvm_emit_constructors_and_destructors(GenContext *c)
{
	if (platform_target.object_format == OBJ_FORMAT_MACHO)
	{
		llvm_emit_macho_xtor(c, c->constructors, "c3ctor");
		llvm_emit_macho_xtor(c, c->destructors, "c3dtor");

		LLVMValueRef runtime_start = LLVMGetNamedFunction(c->module, "__c3_runtime_startup");
		if (!runtime_start || !LLVMGetFirstBasicBlock(runtime_start)) return;
		LLVMValueRef vals[3] = { llvm_const_int(c, type_int, 65535), runtime_start, llvm_get_zero(c, type_voidptr) };
		LLVMValueRef entry = LLVMConstNamedStruct(c->xtor_entry_type, vals, 3);
		LLVMValueRef array = LLVMConstArray(c->xtor_entry_type, &entry, 1);
		LLVMValueRef global_ctor = LLVMAddGlobal(c->module, LLVMTypeOf(array), "llvm.global_ctors");
		LLVMSetLinkage(global_ctor, LLVMAppendingLinkage);
		LLVMSetInitializer(global_ctor, array);
		LLVMValueRef runtime_end = LLVMGetNamedFunction(c->module, "__c3_runtime_finalize");
		if (!runtime_end || !LLVMGetFirstBasicBlock(runtime_end)) error_exit("Failed to find __c3_runtime_finalize in the same module as __c3_runtime_startup.");
		vals[1] = runtime_end;
		entry = LLVMConstNamedStruct(c->xtor_entry_type, vals, 3);
		array = LLVMConstArray(c->xtor_entry_type, &entry, 1);
		LLVMValueRef global_dtor = LLVMAddGlobal(c->module, LLVMTypeOf(array), "llvm.global_dtors");
		LLVMSetLinkage(global_dtor, LLVMAppendingLinkage);
		LLVMSetInitializer(global_dtor, array);
		return;
	}
	llvm_emit_xtor(c, c->constructors, "llvm.global_ctors");
	llvm_emit_xtor(c, c->destructors, "llvm.global_dtors");
}

/**
 * Consider the case when we have int[5] x = { [0] = 1, [1] = 3 }
 * In this case we want this: { i32 0, i32 2, [8 x i32] zeroinitializer }
 * If it's just a single element we don't use [1 x i32] but just i32 0. If it's
 * an array then this is modifying the original type.
 */
static LLVMValueRef llvm_emit_const_array_padding(LLVMTypeRef element_type, IndexDiff diff, bool *modified)
{
	if (diff == 1) return llvm_get_zero_raw(element_type);
	*modified = true;
	return llvm_get_zero_raw(LLVMArrayType(element_type, (unsigned)diff));
}

LLVMValueRef llvm_emit_const_initializer(GenContext *c, ConstInitializer *const_init)
{
	switch (const_init->kind)
	{
		case CONST_INIT_ZERO:
			return llvm_get_zero(c, const_init->type);
		case CONST_INIT_ARRAY_VALUE:
			UNREACHABLE
		case CONST_INIT_ARRAY_FULL:
		{
			bool was_modified = false;
			Type *array_type = const_init->type;
			Type *element_type = array_type->array.base;
			LLVMTypeRef element_type_llvm = llvm_get_type(c, element_type);
			ConstInitializer **elements = const_init->init_array_full;
			assert(array_type->type_kind == TYPE_ARRAY || array_type->type_kind == TYPE_VECTOR);
			ArraySize size = array_type->array.len;
			assert(size > 0);
			LLVMValueRef *parts = VECNEW(LLVMValueRef, size);
			for (MemberIndex i = 0; i < (MemberIndex)size; i++)
			{
				LLVMValueRef element = llvm_emit_const_initializer(c, elements[i]);
				if (element_type_llvm != LLVMTypeOf(element)) was_modified = true;
				vec_add(parts, element);
			}
			if (array_type->type_kind == TYPE_VECTOR)
			{
				return LLVMConstVector(parts, vec_size(parts));
			}
			if (was_modified)
			{
				return llvm_get_unnamed_struct(c, parts, true);
			}
			return llvm_get_array(element_type_llvm, parts, vec_size(parts));
		}

		case CONST_INIT_ARRAY:
		{
			bool was_modified = false;
			Type *array_type = const_init->type;
			Type *element_type = array_type->array.base;
			LLVMTypeRef element_type_llvm = llvm_get_type(c, element_type);
			AlignSize expected_align = llvm_abi_alignment(c, element_type_llvm);
			ConstInitializer **elements = const_init->init_array.elements;
			unsigned element_count = vec_size(elements);
			assert(element_count > 0 && "Array should always have gotten at least one element.");
			MemberIndex current_index = 0;
			unsigned alignment = 0;
			LLVMValueRef *parts = NULL;
			bool pack = false;
			VECEACH(elements, i)
			{
				ConstInitializer *element = elements[i];
				assert(element->kind == CONST_INIT_ARRAY_VALUE);
				MemberIndex element_index = element->init_array_value.index;
				IndexDiff diff = element_index - current_index;
				if (alignment && expected_align != alignment)
				{
					pack = true;
				}
				alignment = expected_align;
				// Add zeroes
				if (diff > 0)
				{
					vec_add(parts, llvm_emit_const_array_padding(element_type_llvm, diff, &was_modified));
				}
				LLVMValueRef value = llvm_emit_const_initializer(c, element->init_array_value.element);
				if (LLVMTypeOf(value) != element_type_llvm) was_modified = true;
				vec_add(parts, value);
				current_index = element_index + 1;
			}

			IndexDiff end_diff = (MemberIndex)array_type->array.len - current_index;
			if (end_diff > 0)
			{
				vec_add(parts, llvm_emit_const_array_padding(element_type_llvm, end_diff, &was_modified));
			}
			if (was_modified)
			{
				return llvm_get_unnamed_struct(c, parts, pack);
			}
			if (type_flat_is_vector(array_type))
			{
				return LLVMConstVector(parts, vec_size(parts));
			}
			return llvm_get_array(element_type_llvm, parts, vec_size(parts));
		}
		case CONST_INIT_UNION:
		{
			Decl *decl = const_init->type->decl;

			// Emit our value.
			LLVMValueRef result = llvm_emit_const_initializer(c, const_init->init_union.element);
			LLVMTypeRef result_type = LLVMTypeOf(result);

			// Get the union value
			LLVMTypeRef union_type_llvm = llvm_get_type(c, decl->type);

			// Take the first type in the union (note that there may be padding!)
			LLVMTypeRef first_type = LLVMStructGetTypeAtIndex(union_type_llvm, 0);

			// We need to calculate some possible padding.
			TypeSize union_size = type_size(const_init->type);
			TypeSize member_size = llvm_abi_size(c, result_type);

			// Create the resulting values:
			LLVMValueRef values[2] = { result, NULL };
			unsigned value_count = 1;

			// Add possible padding as and i8 array.
			if (union_size > member_size)
			{
				values[1] = llvm_emit_const_padding(c, union_size - member_size);
				value_count = 2;
			}

			// Is this another type than usual for the union?
			if (first_type != result_type)
			{
				// Yes, so the type needs to be modified.
				return llvm_get_struct(c, values, value_count);
			}

			return llvm_get_struct_named(union_type_llvm, values, value_count);
		}
		case CONST_INIT_STRUCT:
		{
			if (const_init->type->type_kind == TYPE_BITSTRUCT)
			{
				return llvm_emit_const_bitstruct(c, const_init);
			}
			Decl *decl = const_init->type->decl;
			Decl **members = decl->strukt.members;
			bool is_packed = decl->is_packed;
			uint32_t count = vec_size(members);
			if (decl->decl_kind == DECL_UNION && count) count = 1;
			LLVMValueRef *entries = NULL;
			bool was_modified = false;
			ByteSize prev_size = 0;
			for (MemberIndex i = 0; i < count; i++)
			{
				if (members[i]->padding)
				{
					vec_add(entries, llvm_emit_const_padding(c, members[i]->padding));
				}
				LLVMTypeRef expected_type = llvm_get_type(c, const_init->init_struct[i]->type);
				LLVMValueRef element = llvm_emit_const_initializer(c, const_init->init_struct[i]);
				LLVMTypeRef element_type = LLVMTypeOf(element);
				assert(LLVMIsConstant(element));
				if (expected_type != element_type)
				{
					was_modified = true;
				}
				// We may need to adjust alignment here due to the lack
				// of LLVM union support (while still being strict about structs)
				if (i > 0 && was_modified)
				{
					// Let's look at the old offset.
					ByteSize old_offset = members[i - 1]->offset;
					// What is the expected offset we would get?
					ByteSize new_offset = is_packed ? old_offset + prev_size : aligned_offset(old_offset + prev_size, llvm_abi_alignment(c, element_type));
					// Add the padding we have built in.
					new_offset += members[i]->padding;
					// If this offset is too small, add const padding.
					if (new_offset < members[i]->offset)
					{
						vec_add(entries, llvm_emit_const_padding(c, members[i]->offset - new_offset));
					}
				}
				prev_size = llvm_abi_size(c, element_type);
				vec_add(entries, element);
			}
			if (decl->strukt.padding)
			{
				vec_add(entries, llvm_emit_const_padding(c, decl->strukt.padding));
			}
			if (was_modified)
			{
				return llvm_get_unnamed_struct(c, entries, decl->is_packed);
			}
			return llvm_get_struct_of_type(c, const_init->type, entries, vec_size(entries));
		}
		case CONST_INIT_VALUE:
		{
			BEValue value;
			llvm_emit_expr(c, &value, const_init->init_value);
			return llvm_load_value_store(c, &value);
		}
	}
	UNREACHABLE
}


void llvm_emit_ptr_from_array(GenContext *c, BEValue *value)
{
	switch (value->type->type_kind)
	{
		case TYPE_POINTER:
			llvm_value_rvalue(c, value);
			value->kind = BE_ADDRESS;
			return;
		case TYPE_ARRAY:
		case TYPE_VECTOR:
		case TYPE_FLEXIBLE_ARRAY:
			return;
		case TYPE_SUBARRAY:
		{
			BEValue member;
			llvm_emit_subarray_pointer(c, value, &member);
			llvm_value_rvalue(c, &member);
			llvm_value_set_address(value,
								   member.value,
								   type_get_ptr(value->type->array.base),
								   type_abi_alignment(value->type->array.base));
			return;
		}
		default:
			UNREACHABLE
	}
}

void llvm_set_global_tls(Decl *decl)
{
	if (!decl->var.is_threadlocal) return;
	LLVMThreadLocalMode thread_local_mode = LLVMGeneralDynamicTLSModel;
	if (!decl->var.is_addr && decl_is_local(decl))
	{
		thread_local_mode = LLVMLocalDynamicTLSModel;
	}
	LLVMSetThreadLocal(decl->backend_ref, true);
	LLVMSetThreadLocalMode(decl->backend_ref, thread_local_mode);
	void *optional_ref = decl->var.optional_ref;
	if (optional_ref)
	{
		LLVMSetThreadLocal(optional_ref, true);
		LLVMSetThreadLocalMode(optional_ref, thread_local_mode);
	}
}
void llvm_set_internal_linkage(LLVMValueRef alloc)
{
	LLVMSetLinkage(alloc, LLVMInternalLinkage);
	LLVMSetVisibility(alloc, LLVMDefaultVisibility);
}
void llvm_set_private_linkage(LLVMValueRef alloc)
{
	LLVMSetLinkage(alloc, LLVMPrivateLinkage);
	LLVMSetVisibility(alloc, LLVMDefaultVisibility);
}
void llvm_emit_global_variable_init(GenContext *c, Decl *decl)
{
	assert(decl->var.kind == VARDECL_GLOBAL || decl->var.kind == VARDECL_CONST || decl->var.is_static);

	// Skip real constants.
	if (!decl->type) return;

	LLVMValueRef init_value;

	Type *var_type = type_lowering(decl->type);

	Expr *init_expr = decl->var.init_expr;
	// Fold "source" of the init.
	while (init_expr && init_expr->expr_kind == EXPR_IDENTIFIER)
	{
		Decl *inner_decl = decl_flatten(init_expr->identifier_expr.decl);
		if (inner_decl->decl_kind != DECL_VAR) break;
		if (inner_decl->var.kind != VARDECL_CONST) break;
		init_expr = inner_decl->var.init_expr;
	}
	if (init_expr && init_expr->expr_kind != EXPR_OPTIONAL)
	{
		if (expr_is_const_initializer(init_expr))
		{
			ConstInitializer *list = init_expr->const_expr.initializer;
			init_value = llvm_emit_const_initializer(c, list);
		}
		else
		{
			BEValue value;
			llvm_emit_expr(c, &value, init_expr);
			init_value = llvm_load_value_store(c, &value);
		}
	}
	else
	{
		init_value = decl->var.no_init ? llvm_get_undef(c, var_type) : llvm_get_zero(c, var_type);
	}


	LLVMValueRef old = decl->backend_ref;
	LLVMValueRef global_ref = decl->backend_ref = llvm_add_global_raw(c,
																	  decl_get_extname(decl),
																	  LLVMTypeOf(init_value),
																	  decl->alignment);
	if (decl->var.is_addr)
	{
		LLVMSetUnnamedAddress(global_ref, LLVMNoUnnamedAddr);
	}
	else
	{
		LLVMSetUnnamedAddress(decl->backend_ref,
							  decl_is_local(decl) ? LLVMGlobalUnnamedAddr : LLVMLocalUnnamedAddr);
	}
	if (decl->section_id)
	{
		LLVMSetSection(global_ref, section_from_id(decl->section_id));
	}
	llvm_set_global_tls(decl);

	LLVMValueRef optional_ref = decl->var.optional_ref;
	if (optional_ref)
	{
		llvm_set_alignment(optional_ref, type_alloca_alignment(type_anyfault));
		LLVMSetUnnamedAddress(optional_ref, LLVMGlobalUnnamedAddr);
	}
	LLVMValueRef optional_value = NULL;
	if (init_expr && IS_OPTIONAL(init_expr) && init_expr->expr_kind == EXPR_OPTIONAL)
	{
		Expr *inner = init_expr->inner_expr;
		assert(expr_is_const(inner) && inner->const_expr.const_kind == CONST_ERR);
		BEValue value;
		llvm_emit_expr(c, &value, inner);
		optional_value = llvm_load_value_store(c, &value);
	}
	if (!decl->is_extern)
	{
		LLVMSetInitializer(decl->backend_ref, init_value);
		if (optional_ref)
		{
			LLVMSetInitializer(optional_ref, optional_value ? optional_value : llvm_get_zero(c, type_anyfault));
		}
	}

	LLVMSetGlobalConstant(global_ref, decl->var.kind == VARDECL_CONST);

	if (decl->is_extern)
	{
		LLVMSetLinkage(global_ref, LLVMExternalLinkage);
		if (optional_ref) LLVMSetLinkage(optional_ref, LLVMExternalLinkage);
	}
	else if (decl_is_externally_visible(decl))
	{
		LLVMSetVisibility(global_ref, LLVMDefaultVisibility);
		if (optional_ref) LLVMSetVisibility(optional_ref, LLVMDefaultVisibility);
	}
	else
	{
		if (decl->var.kind == VARDECL_CONST || decl->var.kind == VARDECL_GLOBAL)
		{
			LLVMSetVisibility(global_ref, LLVMProtectedVisibility);
			if (optional_ref) LLVMSetVisibility(optional_ref, LLVMProtectedVisibility);
		}
		else
		{
			LLVMSetLinkage(global_ref, LLVMInternalLinkage);
			if (optional_ref) LLVMSetLinkage(optional_ref, LLVMInternalLinkage);
		}
	}

	decl->backend_ref = global_ref;
	if (old)
	{
		LLVMReplaceAllUsesWith(old, global_ref);
		LLVMDeleteGlobal(old);
	}

	// Should we set linkage here?
	if (llvm_use_debug(c))
	{
		llvm_emit_debug_global_var(c, decl);
	}
}
static void gencontext_verify_ir(GenContext *context)
{
	char *error = NULL;
	assert(context->module);
	if (LLVMVerifyModule(context->module, LLVMPrintMessageAction, &error))
	{
		if (*error)
		{
			puts("IR integrity failure.");
			LLVMDumpModule(context->module);
			error_exit("Could not verify IR: %s", error);
		}
		error_exit("Could not verify module IR.");
	}
}


static void gencontext_emit_object_file(GenContext *context)
{
	char *err = "";
	DEBUG_LOG("Target: %s", platform_target.target_triple);
	LLVMSetTarget(context->module, platform_target.target_triple);
	char *layout = LLVMCopyStringRepOfTargetData(context->target_data);
	LLVMSetDataLayout(context->module, layout);
	LLVMDisposeMessage(layout);

	if (context->asm_filename)
	{
		// Generate .s file
		if (LLVMTargetMachineEmitToFile(context->machine, context->module, (char *)context->asm_filename, LLVMAssemblyFile, &err))
		{
			error_exit("Could not emit asm file: %s", err);
		}
	}

	// Generate .o or .obj file
	if (LLVMTargetMachineEmitToFile(context->machine, context->module, (char *)context->object_filename, LLVMObjectFile, &err))
	{
		error_exit("Could not emit object file: %s", err);
	}

}

static void llvm_emit_asm_file(GenContext *context)
{
	char *err = "";
	DEBUG_LOG("Target: %s", platform_target.target_triple);
	LLVMSetTarget(context->module, platform_target.target_triple);
	char *layout = LLVMCopyStringRepOfTargetData(context->target_data);
	LLVMSetDataLayout(context->module, layout);
	LLVMDisposeMessage(layout);

	// Generate .s file
	if (LLVMTargetMachineEmitToFile(context->machine, context->module, (char *)context->asm_filename, LLVMAssemblyFile, &err))
	{
		error_exit("Could not emit asm file: %s", err);
	}
}

void gencontext_print_llvm_ir(GenContext *context)
{
	char *err = NULL;
	if (LLVMPrintModuleToFile(context->module, context->ir_filename, &err))
	{
		error_exit("Could not emit ir to file: %s", err);
	}
}


LLVMValueRef llvm_emit_alloca(GenContext *c, LLVMTypeRef type, unsigned alignment, const char *name)
{
	assert(!llvm_is_global_eval(c));
	assert(alignment > 0);
	LLVMBasicBlockRef current_block = LLVMGetInsertBlock(c->builder);
	LLVMPositionBuilderBefore(c->builder, c->alloca_point);
	assert(LLVMGetTypeContext(type) == c->context);
	LLVMValueRef alloca = LLVMBuildAlloca(c->builder, type, name);
	llvm_set_alignment(alloca, alignment);
	LLVMPositionBuilderAtEnd(c->builder, current_block);
	return alloca;
}

LLVMValueRef llvm_emit_alloca_aligned(GenContext *c, Type *type, const char *name)
{
	return llvm_emit_alloca(c, llvm_get_type(c, type), type_alloca_alignment(type), name);
}

void llvm_emit_and_set_decl_alloca(GenContext *c, Decl *decl)
{
	Type *type = type_lowering(decl->type);
	if (type == type_void) return;
	assert(!decl->backend_ref && !decl->is_value);
	decl->backend_ref = llvm_emit_alloca(c, llvm_get_type(c, type), decl->alignment, decl->name ? decl->name : ".anon");
}

void llvm_emit_local_var_alloca(GenContext *c, Decl *decl)
{
	assert(!decl->var.is_static && decl->var.kind != VARDECL_CONST);
	llvm_emit_and_set_decl_alloca(c, decl);
	if (llvm_use_debug(c))
	{
		llvm_emit_debug_local_var(c, decl);
	}
}

static inline unsigned lookup_intrinsic(const char *name)
{
	return LLVMLookupIntrinsicID(name, strlen(name));
}

static inline unsigned lookup_attribute(const char *name)
{
	return LLVMGetEnumAttributeKindForName(name, strlen(name));
}

static bool intrinsics_setup = false;
LLVMAttributes attribute_id;
LLVMIntrinsics intrinsic_id;

static void llvm_codegen_setup()
{
	if (intrinsics_setup) return;

	intrinsic_id.abs = lookup_intrinsic("llvm.abs");
	intrinsic_id.assume = lookup_intrinsic("llvm.assume");
	intrinsic_id.bitreverse = lookup_intrinsic("llvm.bitreverse");
	intrinsic_id.bswap = lookup_intrinsic("llvm.bswap");
	intrinsic_id.ceil = lookup_intrinsic("llvm.ceil");
	intrinsic_id.convert_from_fp16 = lookup_intrinsic("llvm.convert.from.fp16");
	intrinsic_id.convert_to_fp16 = lookup_intrinsic("llvm.convert.to.fp16");
	intrinsic_id.copysign = lookup_intrinsic("llvm.copysign");
	intrinsic_id.cos = lookup_intrinsic("llvm.cos");
	intrinsic_id.ctlz = lookup_intrinsic("llvm.ctlz");
	intrinsic_id.ctpop = lookup_intrinsic("llvm.ctpop");
	intrinsic_id.cttz = lookup_intrinsic("llvm.cttz");
	intrinsic_id.exp = lookup_intrinsic("llvm.exp");
	intrinsic_id.exp2 = lookup_intrinsic("llvm.exp2");
	intrinsic_id.expect = lookup_intrinsic("llvm.expect");
	intrinsic_id.expect_with_probability = lookup_intrinsic("llvm.expect.with.probability");
	intrinsic_id.fabs = lookup_intrinsic("llvm.fabs");
	intrinsic_id.floor = lookup_intrinsic("llvm.floor");
	intrinsic_id.fma = lookup_intrinsic("llvm.fma");
	intrinsic_id.frameaddress = lookup_intrinsic("llvm.frameaddress");
	intrinsic_id.fshl = lookup_intrinsic("llvm.fshl");
	intrinsic_id.fshr = lookup_intrinsic("llvm.fshr");
	intrinsic_id.gather = lookup_intrinsic("llvm.masked.gather");
#if LLVM_VERSION_MAJOR < 16
	intrinsic_id.get_rounding = lookup_intrinsic("llvm.flt.rounds");
#else
	intrinsic_id.get_rounding = lookup_intrinsic("llvm.get.rounding");
#endif
	intrinsic_id.lifetime_end = lookup_intrinsic("llvm.lifetime.end");
	intrinsic_id.lifetime_start = lookup_intrinsic("llvm.lifetime.start");
	intrinsic_id.llrint = lookup_intrinsic("llvm.llrint");
	intrinsic_id.llround = lookup_intrinsic("llvm.llround");
	intrinsic_id.log = lookup_intrinsic("llvm.log");
	intrinsic_id.log2 = lookup_intrinsic("llvm.log2");
	intrinsic_id.log10 = lookup_intrinsic("llvm.log10");
	intrinsic_id.lrint = lookup_intrinsic("llvm.lrint");
	intrinsic_id.lround = lookup_intrinsic("llvm.lround");
	intrinsic_id.masked_compressstore = lookup_intrinsic("llvm.masked.compressstore");
	intrinsic_id.masked_expandload = lookup_intrinsic("llvm.masked.expandload");
	intrinsic_id.masked_load = lookup_intrinsic("llvm.masked.load");
	intrinsic_id.masked_store = lookup_intrinsic("llvm.masked.store");
	intrinsic_id.maximum = lookup_intrinsic("llvm.maximum");
	intrinsic_id.maxnum = lookup_intrinsic("llvm.maxnum");
	intrinsic_id.memcpy = lookup_intrinsic("llvm.memcpy");
	intrinsic_id.memcpy_inline = lookup_intrinsic("llvm.memcpy.inline");
	intrinsic_id.memmove = lookup_intrinsic("llvm.memmove");
	intrinsic_id.memset = lookup_intrinsic("llvm.memset");
	intrinsic_id.memset_inline = lookup_intrinsic("llvm.memset.inline");
	intrinsic_id.minimum = lookup_intrinsic("llvm.minimum");
	intrinsic_id.minnum = lookup_intrinsic("llvm.minnum");
	intrinsic_id.fmuladd = lookup_intrinsic("llvm.fmuladd");
	intrinsic_id.nearbyint = lookup_intrinsic("llvm.nearbyint");
	intrinsic_id.pow = lookup_intrinsic("llvm.pow");
	intrinsic_id.powi = lookup_intrinsic("llvm.powi");
	intrinsic_id.prefetch = lookup_intrinsic("llvm.prefetch");
	intrinsic_id.readcyclecounter = lookup_intrinsic("llvm.readcyclecounter");
	intrinsic_id.returnaddress = lookup_intrinsic("llvm.returnaddress");
	intrinsic_id.rint = lookup_intrinsic("llvm.rint");
	intrinsic_id.round = lookup_intrinsic("llvm.round");
	intrinsic_id.roundeven = lookup_intrinsic("llvm.roundeven");
	intrinsic_id.sadd_overflow = lookup_intrinsic("llvm.sadd.with.overflow");
	intrinsic_id.sadd_sat = lookup_intrinsic("llvm.sadd.sat");
	intrinsic_id.scatter = lookup_intrinsic("llvm.masked.scatter");
	intrinsic_id.set_rounding = lookup_intrinsic("llvm.set.rounding");
	intrinsic_id.sin = lookup_intrinsic("llvm.sin");
	intrinsic_id.sshl_sat = lookup_intrinsic("llvm.sshl.sat");
	intrinsic_id.smax = lookup_intrinsic("llvm.smax");
	intrinsic_id.smin = lookup_intrinsic("llvm.smin");
	intrinsic_id.smul_overflow = lookup_intrinsic("llvm.smul.with.overflow");
	intrinsic_id.sqrt = lookup_intrinsic("llvm.sqrt");
	intrinsic_id.ssub_overflow = lookup_intrinsic("llvm.ssub.with.overflow");
	intrinsic_id.ssub_sat = lookup_intrinsic("llvm.ssub.sat");
	intrinsic_id.trap = lookup_intrinsic("llvm.trap");
	intrinsic_id.trunc = lookup_intrinsic("llvm.trunc");
	intrinsic_id.uadd_overflow = lookup_intrinsic("llvm.uadd.with.overflow");
	intrinsic_id.uadd_sat = lookup_intrinsic("llvm.uadd.sat");
	intrinsic_id.umax = lookup_intrinsic("llvm.umax");
	intrinsic_id.umin = lookup_intrinsic("llvm.umin");
	intrinsic_id.umul_overflow = lookup_intrinsic("llvm.umul.with.overflow");
	intrinsic_id.usub_overflow = lookup_intrinsic("llvm.usub.with.overflow");
	intrinsic_id.ushl_sat = lookup_intrinsic("llvm.ushl.sat");
	intrinsic_id.usub_sat = lookup_intrinsic("llvm.usub.sat");
	intrinsic_id.vector_reduce_fmax = lookup_intrinsic("llvm.vector.reduce.fmax");
	intrinsic_id.vector_reduce_fmin = lookup_intrinsic("llvm.vector.reduce.fmin");
	intrinsic_id.vector_reduce_smax = lookup_intrinsic("llvm.vector.reduce.smax");
	intrinsic_id.vector_reduce_smin = lookup_intrinsic("llvm.vector.reduce.smin");
	intrinsic_id.vector_reduce_umax = lookup_intrinsic("llvm.vector.reduce.umax");
	intrinsic_id.vector_reduce_umin = lookup_intrinsic("llvm.vector.reduce.umin");
	intrinsic_id.vector_reduce_add = lookup_intrinsic("llvm.vector.reduce.add");
	intrinsic_id.vector_reduce_fadd = lookup_intrinsic("llvm.vector.reduce.fadd");
	intrinsic_id.vector_reduce_mul = lookup_intrinsic("llvm.vector.reduce.mul");
	intrinsic_id.vector_reduce_fmul = lookup_intrinsic("llvm.vector.reduce.fmul");
	intrinsic_id.vector_reduce_and = lookup_intrinsic("llvm.vector.reduce.and");
	intrinsic_id.vector_reduce_or = lookup_intrinsic("llvm.vector.reduce.or");
	intrinsic_id.vector_reduce_xor = lookup_intrinsic("llvm.vector.reduce.xor");
	intrinsic_id.vector_predicate_select = lookup_intrinsic("llvm.vp.select");
	intrinsic_id.wasm_memory_grow = lookup_intrinsic("llvm.wasm.memory.grow");
	intrinsic_id.wasm_memory_size = lookup_intrinsic("llvm.wasm.memory.size");

	attribute_id.afn = lookup_attribute("afn");
	attribute_id.align = lookup_attribute("align");
	attribute_id.alwaysinline = lookup_attribute("alwaysinline");
	attribute_id.arcp = lookup_attribute("arcp");
	attribute_id.byval = lookup_attribute("byval");
	attribute_id.contract = lookup_attribute("contract");
	attribute_id.elementtype = lookup_attribute("elementtype");
	attribute_id.fast = lookup_attribute("fast");
	attribute_id.inlinehint = lookup_attribute("inlinehint");
	attribute_id.inreg = lookup_attribute("inreg");
	attribute_id.naked = lookup_attribute("naked");
	attribute_id.ninf = lookup_attribute("ninf");
	attribute_id.nnan = lookup_attribute("nnan");
	attribute_id.noalias = lookup_attribute("noalias");
	attribute_id.noinline = lookup_attribute("noinline");
	attribute_id.noreturn = lookup_attribute("noreturn");
	attribute_id.nounwind = lookup_attribute("nounwind");
	attribute_id.nsz = lookup_attribute("nsz");
	attribute_id.optnone = lookup_attribute("optnone");
	attribute_id.readonly = lookup_attribute("readonly");
	attribute_id.reassoc = lookup_attribute("reassoc");
	attribute_id.sext = lookup_attribute("signext");
	attribute_id.sret = lookup_attribute("sret");
	attribute_id.ssp = lookup_attribute("ssp");
	attribute_id.target_features = lookup_attribute("target-features");
	attribute_id.uwtable = lookup_attribute("uwtable");
	attribute_id.writeonly = lookup_attribute("writeonly");
	attribute_id.zext = lookup_attribute("zeroext");
	intrinsics_setup = true;
}

void llvm_set_comdat(GenContext *c, LLVMValueRef global)
{
	if (!platform_target.use_comdat) return;
	LLVMComdatRef comdat = LLVMGetOrInsertComdat(c->module, LLVMGetValueName(global));
	LLVMSetComdatSelectionKind(comdat, LLVMAnyComdatSelectionKind);
	LLVMSetComdat(global, comdat);
}

void llvm_set_linkonce(GenContext *c, LLVMValueRef global)
{
	LLVMSetLinkage(global, LLVMLinkOnceAnyLinkage);
	LLVMSetVisibility(global, LLVMDefaultVisibility);
	llvm_set_comdat(c, global);
}

void llvm_set_weak(GenContext *c, LLVMValueRef global)
{
	LLVMSetLinkage(global, LLVMWeakAnyLinkage);
	LLVMSetVisibility(global, LLVMDefaultVisibility);
	llvm_set_comdat(c, global);
}


void llvm_value_set_int(GenContext *c, BEValue *value, Type *type, uint64_t i)
{
	llvm_value_set(value, llvm_const_int(c, type, i), type);
}

bool llvm_value_is_const(BEValue *value)
{
	return LLVMIsConstant(value->value);
}

void llvm_value_set_decl(GenContext *c, BEValue *value, Decl *decl)
{
	decl = decl_flatten(decl);
	if (decl->is_value)
	{
		llvm_value_set(value, decl->backend_value, decl->type);
		return;
	}
	llvm_value_set_decl_address(c, value, decl);
}


LLVMBasicBlockRef llvm_basic_block_new(GenContext *c, const char *name)
{
	return LLVMCreateBasicBlockInContext(c->context, name);
}

static void llvm_emit_type_decls(GenContext *context, Decl *decl)
{
	switch (decl->decl_kind)
	{
		case NON_TYPE_DECLS:
		case DECL_ERASED:
		case DECL_FNTYPE:
			UNREACHABLE;
		case DECL_TYPEDEF:
			if (decl->typedef_decl.is_func)
			{
				REMINDER("Emit func typeid");
			}
			break;
		case DECL_FUNC:
			// Never directly invoked.
			UNREACHABLE
		case DECL_INTERFACE:
			break;
		case DECL_DISTINCT:
		case DECL_STRUCT:
		case DECL_UNION:
		case DECL_ENUM:
		case DECL_FAULT:
		case DECL_BITSTRUCT:
			llvm_get_typeid(context, decl->type);
			break;
	}
}


static inline void llvm_optimize(GenContext *c)
{
	LLVMPassBuilderOptionsRef options = LLVMCreatePassBuilderOptions();
	LLVMPassBuilderOptionsSetVerifyEach(options, active_target.emit_llvm);
#ifndef NDEBUG
	LLVMPassBuilderOptionsSetDebugLogging(options, debug_log);
#endif
	const char *passes = NULL;
	switch (active_target.optsize)
	{
		case SIZE_OPTIMIZATION_SMALL:
			passes = "default<Os>";
			break;
		case SIZE_OPTIMIZATION_TINY:
			passes = "default<Oz>";
			break;
		default:
			break;
	}
	switch (active_target.optlevel)
	{
		case OPTIMIZATION_NONE:
		case OPTIMIZATION_NOT_SET:
			passes = "default<O0>";
			break;
		case OPTIMIZATION_LESS:
			if (!passes) passes = "default<O1>";
			break;
		case OPTIMIZATION_MORE:
			if (!passes) passes = "default<O2>";
			break;
		case OPTIMIZATION_AGGRESSIVE:
			if (!passes) passes = "default<O3>";
			break;
	}
	LLVMErrorRef err = LLVMRunPasses(c->module, passes, c->machine, options);
	if (err)
	{
		error_exit("An error occurred: %s.", LLVMGetErrorMessage(err));
	}
	LLVMDisposePassBuilderOptions(options);
}

const char *llvm_codegen(void *context)
{
	GenContext *c = context;
	llvm_optimize(c);

	// Serialize the LLVM IR, if requested, also verify the IR in this case
	if (active_target.emit_llvm)
	{
		gencontext_print_llvm_ir(c);
		gencontext_verify_ir(c);
	}

	const char *object_name = NULL;
	if (active_target.emit_object_files)
	{
		gencontext_emit_object_file(c);
		object_name = c->object_filename;
	}

	if (active_target.emit_asm)
	{
		llvm_emit_asm_file(context);
	}

	gencontext_end_module(c);
	gencontext_destroy(c);

	return object_name;
}

void llvm_add_global_decl(GenContext *c, Decl *decl)
{
	assert(decl->var.kind == VARDECL_GLOBAL || decl->var.kind == VARDECL_CONST);

	bool same_module = decl_module(decl) == c->code_module;
	const char *name = same_module ? "temp_global" : decl_get_extname(decl);
	decl->backend_ref = llvm_add_global(c, name, decl->type, decl->alignment);
	llvm_set_alignment(decl->backend_ref, decl->alignment);
	if (!same_module)
	{
		LLVMSetLinkage(decl->backend_ref, LLVMExternalLinkage);
	}
	if (decl->var.kind == VARDECL_CONST)
	{
		LLVMSetGlobalConstant(decl->backend_ref, true);
	}
	if (IS_OPTIONAL(decl))
	{
		scratch_buffer_clear();
		scratch_buffer_append(decl_get_extname(decl));
		scratch_buffer_append(".f");
		decl->var.optional_ref = llvm_add_global(c, scratch_buffer_to_string(), type_anyfault, 0);
	}
	llvm_set_global_tls(decl);
}

LLVMValueRef llvm_get_opt_ref(GenContext *c, Decl *decl)
{
	llvm_get_ref(c, decl);
	decl = decl_flatten(decl);
	if (decl->decl_kind != DECL_VAR) return NULL;
	return decl->var.optional_ref;
}

static void llvm_emit_param_attributes(GenContext *c, LLVMValueRef function, ABIArgInfo *info, bool is_return, int index, int last_index)
{
	assert(last_index == index || info->kind == ABI_ARG_DIRECT_PAIR || info->kind == ABI_ARG_IGNORE
		   || info->kind == ABI_ARG_EXPAND || info->kind == ABI_ARG_DIRECT || info->kind == ABI_ARG_DIRECT_COERCE
		   || info->kind == ABI_ARG_DIRECT_COERCE_INT || info->kind == ABI_ARG_EXPAND_COERCE
		   || info->kind == ABI_ARG_DIRECT_SPLIT_STRUCT_I32);

	if (info->attributes.zeroext)
	{
		// Direct only
		assert(index == last_index);
		llvm_attribute_add(c, function, attribute_id.zext, index);
	}
	if (info->attributes.signext)
	{
		// Direct only
		assert(index == last_index);
		llvm_attribute_add(c, function, attribute_id.sext, index);
	}
	if (info->attributes.by_reg)
	{
		llvm_attribute_add_range(c, function, attribute_id.inreg, index, last_index);
	}
	switch (info->kind)
	{
		case ABI_ARG_EXPAND:
		case ABI_ARG_IGNORE:
		case ABI_ARG_DIRECT_SPLIT_STRUCT_I32:
		case ABI_ARG_DIRECT_COERCE:
		case ABI_ARG_DIRECT_COERCE_INT:
		case ABI_ARG_DIRECT_PAIR:
		case ABI_ARG_DIRECT:
		case ABI_ARG_EXPAND_COERCE:
			break;
		case ABI_ARG_INDIRECT:
			if (is_return)
			{
				assert(info->indirect.type);
				llvm_attribute_add_type(c, function, attribute_id.sret, llvm_get_type(c, info->indirect.type), 1);
				llvm_attribute_add(c, function, attribute_id.noalias, 1);
				llvm_attribute_add_int(c, function, attribute_id.align, info->indirect.alignment, 1);
			}
			else
			{
				if (info->attributes.by_val) llvm_attribute_add_type(c, function, attribute_id.byval, llvm_get_type(c, info->indirect.type), index);
				llvm_attribute_add_int(c, function, attribute_id.align, info->indirect.alignment, index);
			}
			break;
	}

}

void llvm_append_function_attributes(GenContext *c, Decl *decl)
{
	FunctionPrototype *prototype = type_get_resolved_prototype(decl->type);

	LLVMValueRef function = decl->backend_ref;
	ABIArgInfo *ret_abi_info = prototype->ret_abi_info;
	llvm_emit_param_attributes(c, function, ret_abi_info, true, 0, 0);
	unsigned params = vec_size(prototype->param_types);
	if (c->debug.enable_stacktrace)
	{
		llvm_attribute_add_string(c, function, "frame-pointer", "all", -1);
		llvm_attribute_add(c, function, attribute_id.ssp, -1);
	}
	llvm_attribute_add_string(c, function, "stack-protector-buffer-size", "8", -1);
	llvm_attribute_add_string(c, function, "no-trapping-math", "true", -1);

	if (prototype->ret_by_ref)
	{
		ABIArgInfo *info = prototype->ret_by_ref_abi_info;
		llvm_emit_param_attributes(c, function, prototype->ret_by_ref_abi_info, false, info->param_index_start + 1, info->param_index_end);
	}
	for (unsigned i = 0; i < params; i++)
	{
		ABIArgInfo *info = prototype->abi_args[i];
		llvm_emit_param_attributes(c, function, info, false, info->param_index_start + 1, info->param_index_end);
	}
	// We ignore decl->func_decl.attr_inline and place it in every call instead.
	if (decl->func_decl.attr_noinline)
	{
		llvm_attribute_add(c, function, attribute_id.noinline, -1);
	}
	if (decl->func_decl.signature.attrs.noreturn)
	{
		llvm_attribute_add(c, function, attribute_id.noreturn, -1);
	}
	if (decl->is_export && arch_is_wasm(platform_target.arch))
	{
		if (c->code_module == decl_module(decl))
		{
			llvm_attribute_add_string(c, function, "wasm-export-name", decl_get_extname(decl), -1);
		}
	}
	if (decl->is_extern && arch_is_wasm(platform_target.arch))
	{
		llvm_attribute_add_string(c, function, "wasm-import-name", decl_get_extname(decl), -1);
	}
	if (decl->alignment != type_abi_alignment(decl->type))
	{
		llvm_set_alignment(function, decl->alignment);
	}
	llvm_attribute_add(c, function, attribute_id.nounwind, -1);
	llvm_attribute_add_int(c, function, attribute_id.uwtable, UWTABLE, -1);

	if (decl->func_decl.attr_naked)
	{
		llvm_attribute_add(c, function, attribute_id.naked, -1);
	}
	LLVMSetFunctionCallConv(function, llvm_call_convention_from_call(prototype->call_abi));
}
LLVMValueRef llvm_get_ref(GenContext *c, Decl *decl)
{
	assert(!decl->is_value);
	LLVMValueRef backend_ref = decl->backend_ref;
	if (backend_ref)
	{
		if (!LLVMIsAGlobalValue(backend_ref) || LLVMGetGlobalParent(backend_ref) == c->module) return backend_ref;
	}
	switch (decl->decl_kind)
	{
		case DECL_ERASED:
		case DECL_FNTYPE:
			UNREACHABLE
		case DECL_VAR:
			if (decl->var.kind == VARDECL_UNWRAPPED)
			{
				return decl->backend_ref = llvm_get_ref(c, decl->var.alias);
			}
			assert(decl->var.kind == VARDECL_GLOBAL || decl->var.kind == VARDECL_CONST);
			llvm_add_global_decl(c, decl);
			if (decl->is_export && platform_target.os == OS_TYPE_WIN32 && !active_target.win.def)
			{
				LLVMSetDLLStorageClass(decl->backend_ref, LLVMDLLExportStorageClass);
			}
			return decl->backend_ref;
		case DECL_FUNC:
			if (decl->func_decl.attr_interface_method)
			{
				return decl->backend_ref = llvm_get_selector(c, decl->name);
			}
			backend_ref = decl->backend_ref = LLVMAddFunction(c->module, decl_get_extname(decl), llvm_get_type(c, decl->type));
			llvm_append_function_attributes(c, decl);
			if (decl->is_export && platform_target.os == OS_TYPE_WIN32  && !active_target.win.def && decl->name != kw_main && decl->name != kw_mainstub)
			{
				LLVMSetDLLStorageClass(backend_ref, LLVMDLLExportStorageClass);
			}
			if (decl_is_local(decl))
			{
				assert(decl_module(decl) == c->code_module);
				llvm_set_internal_linkage(backend_ref);
			}
			return backend_ref;
		case DECL_DEFINE:
			return llvm_get_ref(c, decl->define_decl.alias);
		case DECL_FAULTVALUE:
			if (!decl->backend_ref)
			{
				llvm_get_typeid(c, declptr(decl->enum_constant.parent)->type);
			}
			assert(decl->backend_ref);
			return decl->backend_ref;
		case DECL_POISONED:
		case DECL_ATTRIBUTE:
		case DECL_BITSTRUCT:
		case DECL_CT_ASSERT:
		case DECL_DISTINCT:
		case DECL_ENUM:
		case DECL_ENUM_CONSTANT:
		case DECL_FAULT:
		case DECL_IMPORT:
		case DECL_LABEL:
		case DECL_MACRO:
		case DECL_STRUCT:
		case DECL_TYPEDEF:
		case DECL_UNION:
		case DECL_DECLARRAY:
		case DECL_BODYPARAM:
		case DECL_CT_ECHO:
		case DECL_CT_EXEC:
		case DECL_CT_INCLUDE:
		case DECL_GLOBALS:
		case DECL_INTERFACE:
			UNREACHABLE;
	}
	UNREACHABLE
}

static void llvm_gen_test_main(GenContext *c)
{
	Decl *test_runner = global_context.test_func;
	if (!test_runner)
	{
		error_exit("No test runner found.");
	}
	assert(!global_context.main && "Main should not be set if a test main is generated.");
	global_context.main = test_runner;
	LLVMTypeRef cint = llvm_get_type(c, type_cint);
	LLVMTypeRef main_type = LLVMFunctionType(cint, NULL, 0, true);
	LLVMTypeRef runner_type = LLVMFunctionType(c->byte_type, NULL, 0, true);
	LLVMValueRef func = LLVMAddFunction(c->module, kw_main, main_type);
	LLVMValueRef other_func = LLVMAddFunction(c->module, test_runner->extname, runner_type);
	LLVMBasicBlockRef entry = LLVMAppendBasicBlockInContext(c->context, func, "entry");
	LLVMBuilderRef builder = llvm_create_builder(c);
	LLVMPositionBuilderAtEnd(builder, entry);
	LLVMValueRef val = LLVMBuildCall2(builder, runner_type, other_func, NULL, 0, "");
	val = LLVMBuildSelect(builder, LLVMBuildTrunc(builder, val, c->bool_type, ""),
						  LLVMConstNull(cint), LLVMConstInt(cint, 1, false), "");
	LLVMBuildRet(builder, val);
	LLVMDisposeBuilder(builder);
}

INLINE GenContext *llvm_gen_tests(Module** modules, unsigned module_count, LLVMContextRef shared_context)
{
	Path *test_path = path_create_from_string("_$test", 5, INVALID_SPAN);
	Module *test_module = compiler_find_or_create_module(test_path, NULL);

	GenContext *c = cmalloc(sizeof(GenContext));
	active_target.debug_info = DEBUG_INFO_NONE;
	gencontext_init(c, test_module, shared_context);
	gencontext_begin_module(c);

	LLVMValueRef *names = NULL;
	LLVMValueRef *decls = NULL;

	LLVMTypeRef opt_test = LLVMFunctionType(llvm_get_type(c, type_anyfault), NULL, 0, false);
	for (unsigned i = 0; i < module_count; i++)
	{
		Module *module = modules[i];
		FOREACH_BEGIN(Decl *test, module->tests)
			LLVMValueRef ref;
			LLVMTypeRef type = opt_test;
			ref = LLVMAddFunction(c->module, test->extname, type);
			scratch_buffer_clear();
			scratch_buffer_printf("%s::%s", module->name->module, test->name);
			LLVMValueRef name = llvm_emit_string_const(c, scratch_buffer_to_string(), ".test.name");
			vec_add(names, name);
			vec_add(decls, ref);
		FOREACH_END();
	}
	unsigned test_count = vec_size(decls);
	LLVMValueRef name_ref;
	LLVMValueRef decl_ref;

	if (test_count)
	{
		LLVMValueRef array_of_names = LLVMConstArray(c->chars_type, names, test_count);
		LLVMValueRef array_of_decls = LLVMConstArray(c->ptr_type, decls, test_count);
		LLVMTypeRef arr_type = LLVMTypeOf(array_of_names);
		name_ref = llvm_add_global_raw(c, ".test_names", arr_type, 0);
		decl_ref = llvm_add_global_raw(c, ".test_decls", LLVMTypeOf(array_of_decls), 0);
		llvm_set_internal_linkage(name_ref);
		llvm_set_internal_linkage(decl_ref);
		LLVMSetGlobalConstant(name_ref, 1);
		LLVMSetGlobalConstant(decl_ref, 1);
		LLVMSetInitializer(name_ref, array_of_names);
		LLVMSetInitializer(decl_ref, array_of_decls);
	}
	else
	{
		name_ref = LLVMConstNull(c->ptr_type);
		decl_ref = LLVMConstNull(c->ptr_type);
	}
	LLVMValueRef count = llvm_const_int(c, type_usz, test_count);
	Type *chars_array = type_get_subarray(type_chars);
	LLVMValueRef name_list = llvm_add_global(c, test_names_var_name, chars_array, type_alloca_alignment(chars_array));
	LLVMSetGlobalConstant(name_list, 1);
	LLVMSetInitializer(name_list, llvm_emit_aggregate_two(c, chars_array, name_ref, count));
	Type *decls_array_type = type_get_subarray(type_voidptr);
	LLVMValueRef decl_list = llvm_add_global(c, test_fns_var_name, decls_array_type, type_alloca_alignment(decls_array_type));
	LLVMSetGlobalConstant(decl_list, 1);
	LLVMSetInitializer(decl_list, llvm_emit_aggregate_two(c, decls_array_type, decl_ref, count));

	if (active_target.type == TARGET_TYPE_TEST)
	{
		llvm_gen_test_main(c);
	}

	if (llvm_use_debug(c))
	{
		LLVMDIBuilderFinalize(c->debug.builder);
		LLVMDisposeDIBuilder(c->debug.builder);
	}
	return c;
}

static void llvm_gen_benchmark_main(GenContext *c)
{
	Decl *benchmark_runner = global_context.benchmark_func;
	if (!benchmark_runner)
	{
		error_exit("No benchmark runner found.");
	}
	assert(!global_context.main && "Main should not be set if a benchmark main is generated.");
	global_context.main = benchmark_runner;
	LLVMTypeRef cint = llvm_get_type(c, type_cint);
	LLVMTypeRef main_type = LLVMFunctionType(cint, NULL, 0, true);
	LLVMTypeRef runner_type = LLVMFunctionType(c->byte_type, NULL, 0, true);
	LLVMValueRef func = LLVMAddFunction(c->module, kw_main, main_type);
	LLVMValueRef other_func = LLVMAddFunction(c->module, benchmark_runner->extname, runner_type);
	LLVMBasicBlockRef entry = LLVMAppendBasicBlockInContext(c->context, func, "entry");
	LLVMBuilderRef builder = llvm_create_builder(c);
	LLVMPositionBuilderAtEnd(builder, entry);
	LLVMValueRef val = LLVMBuildCall2(builder, runner_type, other_func, NULL, 0, "");
	val = LLVMBuildSelect(builder, LLVMBuildTrunc(builder, val, c->bool_type, ""),
						  LLVMConstNull(cint), LLVMConstInt(cint, 1, false), "");
	LLVMBuildRet(builder, val);
	LLVMDisposeBuilder(builder);
}

INLINE GenContext *llvm_gen_benchmarks(Module** modules, unsigned module_count, LLVMContextRef shared_context)
{
	Path *benchmark_path = path_create_from_string("$benchmark", 10, INVALID_SPAN);
	Module *benchmark_module = compiler_find_or_create_module(benchmark_path, NULL);

	GenContext *c = cmalloc(sizeof(GenContext));
	active_target.debug_info = DEBUG_INFO_NONE;
	gencontext_init(c, benchmark_module, shared_context);
	gencontext_begin_module(c);

	LLVMValueRef *names = NULL;
	LLVMValueRef *decls = NULL;

	LLVMTypeRef opt_benchmark = LLVMFunctionType(llvm_get_type(c, type_anyfault), NULL, 0, false);
	for (unsigned i = 0; i < module_count; i++)
	{
		Module *module = modules[i];
		FOREACH_BEGIN(Decl *benchmark, module->benchmarks)
			LLVMValueRef ref;
			LLVMTypeRef type = opt_benchmark;
			ref = LLVMAddFunction(c->module, benchmark->extname, type);
			scratch_buffer_clear();
			scratch_buffer_printf("%s::%s", module->name->module, benchmark->name);
			LLVMValueRef name = llvm_emit_string_const(c, scratch_buffer_to_string(), ".benchmark.name");
			vec_add(names, name);
			vec_add(decls, ref);
		FOREACH_END();
	}
	unsigned benchmark_count = vec_size(decls);
	LLVMValueRef name_ref;
	LLVMValueRef decl_ref;

	if (benchmark_count)
	{
		LLVMValueRef array_of_names = LLVMConstArray(c->chars_type, names, benchmark_count);
		LLVMValueRef array_of_decls = LLVMConstArray(c->ptr_type, decls, benchmark_count);
		LLVMTypeRef arr_type = LLVMTypeOf(array_of_names);
		name_ref = llvm_add_global_raw(c, ".benchmark_names", arr_type, 0);
		decl_ref = llvm_add_global_raw(c, ".benchmark_decls", LLVMTypeOf(array_of_decls), 0);
		llvm_set_internal_linkage(name_ref);
		llvm_set_internal_linkage(decl_ref);
		LLVMSetGlobalConstant(name_ref, 1);
		LLVMSetGlobalConstant(decl_ref, 1);
		LLVMSetInitializer(name_ref, array_of_names);
		LLVMSetInitializer(decl_ref, array_of_decls);
	}
	else
	{
		name_ref = LLVMConstNull(c->ptr_type);
		decl_ref = LLVMConstNull(c->ptr_type);
	}
	LLVMValueRef count = llvm_const_int(c, type_usz, benchmark_count);
	Type *chars_array = type_get_subarray(type_chars);
	LLVMValueRef name_list = llvm_add_global(c, benchmark_names_var_name, chars_array, type_alloca_alignment(chars_array));
	LLVMSetGlobalConstant(name_list, 1);
	LLVMSetInitializer(name_list, llvm_emit_aggregate_two(c, chars_array, name_ref, count));
	Type *decls_array_type = type_get_subarray(type_voidptr);
	LLVMValueRef decl_list = llvm_add_global(c, benchmark_fns_var_name, decls_array_type, type_alloca_alignment(decls_array_type));
	LLVMSetGlobalConstant(decl_list, 1);
	LLVMSetInitializer(decl_list, llvm_emit_aggregate_two(c, decls_array_type, decl_ref, count));

	if (active_target.type == TARGET_TYPE_BENCHMARK)
	{
		llvm_gen_benchmark_main(c);
	}

	if (llvm_use_debug(c))
	{
		LLVMDIBuilderFinalize(c->debug.builder);
		LLVMDisposeDIBuilder(c->debug.builder);
	}
	return c;
}

void **llvm_gen(Module** modules, unsigned module_count)
{
	if (!module_count) return NULL;
	GenContext **gen_contexts = NULL;
	llvm_codegen_setup();
	if (active_target.single_module == SINGLE_MODULE_ON)
	{
		GenContext *first_context;
		unsigned first_element;
		LLVMContextRef context = LLVMGetGlobalContext();
		for (int i = 0; i < module_count; i++)
		{
			GenContext *result = llvm_gen_module(modules[i], context);
			if (!result) continue;
			vec_add(gen_contexts, result);
		}
		if (!gen_contexts) return NULL;
		GenContext *first = gen_contexts[0];
		if (active_target.benchmarking)
		{
			vec_add(gen_contexts, llvm_gen_benchmarks(modules, module_count, context));
		}
		if (active_target.testing)
		{
			vec_add(gen_contexts, llvm_gen_tests(modules, module_count, context));
		}
		unsigned count = vec_size(gen_contexts);
		for (unsigned i = 1; i < count; i++)
		{
			GenContext *other = gen_contexts[i];
			LLVMLinkModules2(first->module, other->module);
			gencontext_destroy(other);
		}
		vec_resize(gen_contexts, 1);
		return (void**)gen_contexts;
	}
	for (unsigned i = 0; i < module_count; i++)
	{
			GenContext *result = llvm_gen_module(modules[i], NULL);
			if (!result) continue;
			vec_add(gen_contexts, result);
	}
	if (active_target.benchmarking)
	{
		vec_add(gen_contexts, llvm_gen_benchmarks(modules, module_count, NULL));
	}
	if (active_target.testing)
	{
		vec_add(gen_contexts, llvm_gen_tests(modules, module_count, NULL));
	}
	return (void**)gen_contexts;
}

LLVMMetadataRef llvm_get_debug_file(GenContext *c, FileId file_id)
{
	VECEACH(c->debug.debug_files, i)
	{
		DebugFile *ref = &c->debug.debug_files[i];
		if (ref->file_id == file_id) return ref->debug_file;
	}
	File *f = source_file_by_id(file_id);
	LLVMMetadataRef file = LLVMDIBuilderCreateFile(c->debug.builder,
												   f->name,
												   strlen(f->name),
												   f->dir_path,
												   strlen(f->dir_path));
	DebugFile debug_file = { .file_id = file_id, .debug_file = file };
	vec_add(c->debug.debug_files, debug_file);
	return file;
}

static bool module_is_stdlib(Module *module)
{
	if (module->name->len < 3) return false;
	if (module->name->len == 3 && strcmp(module->name->module, "std") == 0) return true;
	if (module->name->len > 5 && memcmp(module->name->module, "std::", 5) == 0) return true;
	if (module->name->len == 4 && strcmp(module->name->module, "libc") == 0) return true;
	if (module->name->len > 6 && memcmp(module->name->module, "libc::", 6) == 0) return true;
	return false;
}

static GenContext *llvm_gen_module(Module *module, LLVMContextRef shared_context)
{
	if (!vec_size(module->units)) return NULL;
	if (active_target.emit_stdlib == EMIT_STDLIB_OFF && module_is_stdlib(module)) return NULL;

	assert(intrinsics_setup);

	bool has_elements = false;
	GenContext *gen_context = cmalloc(sizeof(GenContext));
	gencontext_init(gen_context, module, shared_context);
	gencontext_begin_module(gen_context);

	bool only_used = strip_unused();

	FOREACH_BEGIN(CompilationUnit *unit, module->units)

		gencontext_init_file_emit(gen_context, unit);
		gen_context->debug.compile_unit = unit->llvm.debug_compile_unit;
		gen_context->debug.file = (DebugFile){
				.debug_file = unit->llvm.debug_file,
				.file_id = unit->file->file_id };

		FOREACH_BEGIN(Decl *method, unit->methods)
			if (only_used && !method->is_live) continue;
			llvm_emit_function_decl(gen_context, method);
		FOREACH_END();

		FOREACH_BEGIN(Decl *type_decl, unit->types)
			if (only_used && !type_decl->is_live) continue;
			llvm_emit_type_decls(gen_context, type_decl);
		FOREACH_END();

		FOREACH_BEGIN(Decl *enum_decl, unit->enums)
			if (only_used && !enum_decl->is_live) continue;
			llvm_emit_type_decls(gen_context, enum_decl);
		FOREACH_END();

		FOREACH_BEGIN(Decl *func, unit->functions)
			if (only_used && !func->is_live) continue;
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
			llvm_emit_function_decl(gen_context, func);
		FOREACH_END();


		FOREACH_BEGIN(Decl *func, unit->lambdas)
			if (only_used && !func->is_live) continue;
			has_elements = true;
			llvm_emit_function_decl(gen_context, func);
		FOREACH_END();

		if (active_target.type != TARGET_TYPE_TEST && active_target.type != TARGET_TYPE_BENCHMARK && unit->main_function && unit->main_function->is_synthetic)
		{
			has_elements = true;
			llvm_emit_function_decl(gen_context, unit->main_function);
		}

	FOREACH_END();

	FOREACH_BEGIN(CompilationUnit *unit, module->units)

		gen_context->debug.compile_unit = unit->llvm.debug_compile_unit;
		gen_context->debug.file = (DebugFile){
				.debug_file = unit->llvm.debug_file,
				.file_id = unit->file->file_id };

		FOREACH_BEGIN(Decl *var, unit->vars)
			if (only_used && !var->is_live) continue;
			has_elements = true;
			llvm_get_ref(gen_context, var);
		FOREACH_END();

		FOREACH_BEGIN(Decl *var, unit->vars)
			if (only_used && !var->is_live) continue;
			has_elements = true;
			llvm_emit_global_variable_init(gen_context, var);
		FOREACH_END();

		FOREACH_BEGIN(Decl *decl, unit->functions)
			if (decl->func_decl.attr_test && !active_target.testing) continue;
			if (decl->func_decl.attr_benchmark && !active_target.benchmarking) continue;
			if (only_used && !decl->is_live) continue;
			if (decl->func_decl.body)
			{
				has_elements = true;
				llvm_emit_function_body(gen_context, decl);
			}
		FOREACH_END();

		FOREACH_BEGIN(Decl *func, unit->lambdas)
			if (only_used && !func->is_live) continue;
			has_elements = true;
			llvm_emit_function_body(gen_context, func);
		FOREACH_END();

		if (active_target.type != TARGET_TYPE_TEST && active_target.type != TARGET_TYPE_BENCHMARK && unit->main_function && unit->main_function->is_synthetic)
		{
			has_elements = true;
			llvm_emit_function_body(gen_context, unit->main_function);
		}

		FOREACH_BEGIN(Decl *decl, unit->methods)
			if (only_used && !decl->is_live) continue;
			if (!decl->func_decl.body) continue;
			has_elements = true;
			llvm_emit_function_body(gen_context, decl);
		FOREACH_END();

		gencontext_end_file_emit(gen_context, unit);

	FOREACH_END();

	llvm_emit_dynamic_functions(gen_context, gen_context->dynamic_functions);

	llvm_emit_constructors_and_destructors(gen_context);

	if (llvm_use_debug(gen_context))
	{
		LLVMDIBuilderFinalize(gen_context->debug.builder);
		LLVMDisposeDIBuilder(gen_context->debug.builder);
	}

	// If it's in test or benchmark, then we want to serialize the IR before it is optimized.
	if (active_target.test_output || active_target.benchmark_output)
	{
		gencontext_print_llvm_ir(gen_context);
		gencontext_verify_ir(gen_context);
	}
	if (!has_elements) return NULL;
	return gen_context;
}

void llvm_attribute_add_int(GenContext *context, LLVMValueRef value_to_add_attribute_to, unsigned attribute, uint64_t val, int index)
{
	LLVMAttributeRef llvm_attr = LLVMCreateEnumAttribute(context->context, attribute, val);
	LLVMAddAttributeAtIndex(value_to_add_attribute_to, (LLVMAttributeIndex)index, llvm_attr);
}

void llvm_attribute_add_type(GenContext *c, LLVMValueRef value_to_add_attribute_to, unsigned attribute, LLVMTypeRef type, int index)
{
	LLVMAttributeRef llvm_attr = LLVMCreateTypeAttribute(c->context, attribute, type);
	LLVMAddAttributeAtIndex(value_to_add_attribute_to, (LLVMAttributeIndex)index, llvm_attr);
}

void llvm_attribute_add(GenContext *context, LLVMValueRef value_to_add_attribute_to, unsigned attribute, int index)
{
	llvm_attribute_add_int(context, value_to_add_attribute_to, attribute, 0, index);
}

void llvm_attribute_add_call_type(GenContext *c, LLVMValueRef call, unsigned attribute, int index, LLVMTypeRef type)
{
	LLVMAttributeRef llvm_attr = LLVMCreateTypeAttribute(c->context, attribute, type);
	LLVMAddCallSiteAttribute(call, (LLVMAttributeIndex)index, llvm_attr);
}

void llvm_attribute_add_call(GenContext *context, LLVMValueRef call, unsigned attribute, int index, int64_t value)
{
	LLVMAttributeRef llvm_attr = LLVMCreateEnumAttribute(context->context, attribute, (uint64_t)value);
	LLVMAddCallSiteAttribute(call, (LLVMAttributeIndex)index, llvm_attr);
}

void llvm_attribute_add_range(GenContext *context, LLVMValueRef value_to_add_attribute_to, unsigned attribute, int index_start, int index_end)
{
	for (int i = index_start; i <= index_end; i++)
	{
		llvm_attribute_add_int(context, value_to_add_attribute_to, attribute, 0, i);
	}
}

void llvm_attribute_add_string(GenContext *context, LLVMValueRef value_to_add_attribute_to, const char *attribute, const char *value, int index)
{
	LLVMAttributeRef llvm_attr = LLVMCreateStringAttribute(context->context, attribute, (unsigned)strlen(attribute), value, (unsigned)strlen(value));
	LLVMAddAttributeAtIndex(value_to_add_attribute_to, (LLVMAttributeIndex)index, llvm_attr);
}

BitSize llvm_bitsize(GenContext *c, LLVMTypeRef type)
{
	return LLVMSizeOfTypeInBits(c->target_data, type);
}
TypeSize llvm_abi_size(GenContext *c, LLVMTypeRef type)
{
	return (TypeSize)LLVMABISizeOfType(c->target_data, type);
}

AlignSize llvm_abi_alignment(GenContext *c, LLVMTypeRef type)
{
	return (AlignSize)LLVMABIAlignmentOfType(c->target_data, type);
}

LLVMValueRef llvm_emit_memcpy(GenContext *c, LLVMValueRef dest, unsigned dest_align, LLVMValueRef source, unsigned src_align, uint64_t len)
{
	assert(dest_align && src_align);
	if (len <= UINT32_MAX)
	{
		return LLVMBuildMemCpy(c->builder, dest, dest_align, source, src_align, llvm_const_int(c, type_uint, len));
	}
	return LLVMBuildMemCpy(c->builder, dest, dest_align, source, src_align, llvm_const_int(c, type_ulong, len));
}

void llvm_emit_memcpy_to_decl(GenContext *c, Decl *decl, LLVMValueRef source, unsigned source_alignment)
{
	if (source_alignment == 0) source_alignment = type_abi_alignment(decl->type);
	assert(!decl->is_value);
	llvm_emit_memcpy(c, decl->backend_ref, decl->alignment, source, source_alignment, type_size(decl->type));
}

TypeSize llvm_store_size(GenContext *c, LLVMTypeRef type)
{
	return (TypeSize)LLVMStoreSizeOfType(c->target_data, type);
}

TypeSize llvm_alloc_size(GenContext *c, LLVMTypeRef type)
{
	return (TypeSize)aligned_offset((AlignSize)LLVMStoreSizeOfType(c->target_data, type), llvm_abi_alignment(c, type));
}

