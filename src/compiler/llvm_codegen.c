// Copyright (c) 2019-2025 Christoffer Lerno. All rights reserved.
// Use of this source code is governed by the GNU LGPLv3.0 license
// a copy of which can be found in the LICENSE file.

#include "llvm_codegen_internal.h"
#include "compiler_tests/benchmark.h"
#include "c3_llvm.h"
#include <llvm-c/Comdat.h>
#include <llvm-c/Linker.h>
#include <llvm-c/Transforms/PassBuilder.h>
const char *varargslots_name = "varargslots";
const char *temp_name = "$$temp";

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

bool module_should_weaken(Module *module)
{
	if (module->generic_module) return true;
	Module *top = module->top_module;
	return top && (top->name->module == kw_std || top->name->module == kw_libc);
}

static void gencontext_init(GenContext *context, Module *module, LLVMContextRef shared_context)
{
	ASSERT(LLVMIsMultithreaded());
	memset(context, 0, sizeof(GenContext));
	context->weaken = module_should_weaken(module);

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
	if (!compiler.build.emit_llvm && !compiler.build.test_output && !compiler.build.benchmark_output ) LLVMContextSetDiscardValueNames(context->context, true);
	context->code_module = module;
}

static void gencontext_destroy(GenContext *context)
{
	ASSERT(llvm_is_global_eval(context));
	LLVMDisposeBuilder(context->global_builder);
	if (!context->shared_context) LLVMContextDispose(context->context);
	LLVMDisposeTargetData(context->target_data);
	LLVMDisposeTargetMachine(context->machine);
	free(context);
}

LLVMBuilderRef llvm_create_builder(GenContext *c)
{
	LLVMBuilderRef builder = LLVMCreateBuilderInContext(c->context);
	LLVMBuilderSetFastMathFlags(builder, (FastMathOption)compiler.build.feature.fp_math);
	return builder;
}

LLVMValueRef llvm_emit_is_no_opt(GenContext *c, LLVMValueRef error_value)
{
	LLVMValueRef compare = LLVMBuildICmp(c->builder, LLVMIntEQ, error_value, llvm_get_zero(c, type_fault), "not_err");
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
	llvm_set_selector_linkage(c, selector);
	return selector;
}


static LLVMValueRef llvm_emit_macho_xtor_use(GenContext *c, LLVMValueRef global, const char *name)
{
	LLVMValueRef initializer = LLVMAddFunction(c->module, name, c->xtor_func_type);
	LLVMSetLinkage(initializer, LLVMInternalLinkage);
	LLVMSetAlignment(initializer, 8);
	LLVMValueRef vals_fn[3] = { llvm_const_int(c, type_int, 1), initializer, llvm_get_zero(c, type_voidptr) };
	LLVMValueRef result = LLVMConstNamedStruct(c->xtor_entry_type, vals_fn, 3);

	LLVMBasicBlockRef last_block;
	LLVMBuilderRef builder = llvm_create_function_entry(c, initializer, &last_block);
	LLVMValueRef load = LLVMBuildLoad2(builder, LLVMTypeOf(LLVMGetInitializer(global)), global, ".retain_global");
	LLVMSetVolatile(load, true);
	LLVMBuildRetVoid(builder);
	LLVMDisposeBuilder(builder);
	return result;
}

static LLVMValueRef llvm_emit_macho_xtor(GenContext *c, LLVMValueRef *list, const char *name, const char *retain_name)
{
	unsigned len = vec_size(list);
	if (!len) return NULL;
	scratch_buffer_clear();
	scratch_buffer_append(".list$");
	scratch_buffer_append(name);
	LLVMValueRef array = LLVMConstArray(c->xtor_entry_type, list, vec_size(list));
	LLVMValueRef global = LLVMAddGlobal(c->module, LLVMTypeOf(array), scratch_buffer_to_string());
	LLVMSetNoSanitizeAddress(global);
	scratch_buffer_clear();
	scratch_buffer_append("__DATA,__");
	scratch_buffer_append(name);
	LLVMSetLinkage(global, LLVMInternalLinkage);
	LLVMSetInitializer(global, array);
	LLVMSetSection(global, scratch_buffer_to_string());
	LLVMSetAlignment(global, llvm_abi_alignment(c, c->xtor_entry_type));

	return llvm_emit_macho_xtor_use(c, global, retain_name);
}

void llvm_emit_constructors_and_destructors(GenContext *c)
{
	if (compiler.platform.object_format == OBJ_FORMAT_MACHO)
	{

		LLVMValueRef c3_dynamic = LLVMGetNamedGlobal(c->module, "$c3_dynamic");
		LLVMValueRef dtor_global = llvm_emit_macho_xtor(c, c->constructors, "c3ctor", ".c3_ctor_retain");
		LLVMValueRef ctor_global = llvm_emit_macho_xtor(c, c->destructors, "c3dtor", ".c3_dtor_retain");

		LLVMValueRef runtime_start = LLVMGetNamedFunction(c->module, "__c3_runtime_startup");
		int len = 0;
		LLVMValueRef ctors[5];
		if (ctor_global) ctors[len++] = ctor_global;
		if (dtor_global) ctors[len++] = dtor_global;
		if (c3_dynamic) ctors[len++] = llvm_emit_macho_xtor_use(c, c3_dynamic, ".c3_dynamic_retain");
		if (!runtime_start || !LLVMGetFirstBasicBlock(runtime_start))
		{
			goto EMIT_CTORS;
		}
		LLVMValueRef runtime_end = LLVMGetNamedFunction(c->module, "__c3_runtime_finalize");
		if (!runtime_end || !LLVMGetFirstBasicBlock(runtime_end)) error_exit("Failed to find __c3_runtime_finalize in the same module as __c3_runtime_startup.");
		LLVMValueRef vals[3] = { llvm_const_int(c, type_int, 65535), runtime_end, llvm_get_zero(c, type_voidptr) };
		LLVMValueRef entry = LLVMConstNamedStruct(c->xtor_entry_type, vals, 3);
		LLVMValueRef array = LLVMConstArray(c->xtor_entry_type, &entry, 1);
		LLVMValueRef global_dtor = LLVMAddGlobal(c->module, LLVMTypeOf(array), "llvm.global_dtors");
		LLVMSetNoSanitizeAddress(global_dtor);
		LLVMSetLinkage(global_dtor, LLVMAppendingLinkage);
		LLVMSetInitializer(global_dtor, array);
		vals[1] = runtime_start;
		entry = LLVMConstNamedStruct(c->xtor_entry_type, vals, 3);
		ctors[len++] = entry;
EMIT_CTORS:
		if (len == 0) return;
		array = LLVMConstArray(c->xtor_entry_type, ctors, len);
		LLVMValueRef global_ctor = LLVMAddGlobal(c->module, LLVMTypeOf(array), "llvm.global_ctors");
		LLVMSetNoSanitizeAddress(global_ctor);
		LLVMSetLinkage(global_ctor, LLVMAppendingLinkage);
		LLVMSetInitializer(global_ctor, array);
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
	ASSERT(const_init->type == type_flatten(const_init->type));
	switch (const_init->kind)
	{
		case CONST_INIT_ZERO:
			return llvm_get_zero(c, const_init->type);
		case CONST_INIT_ARRAY_VALUE:
			UNREACHABLE
		case CONST_INIT_ARRAY_FULL:
		{
			ASSERT(const_init->type->type_kind != TYPE_SLICE);
			bool was_modified = false;
			Type *array_type = const_init->type;
			Type *element_type = array_type->array.base;
			LLVMTypeRef element_type_llvm = llvm_get_type(c, element_type);
			ConstInitializer **elements = const_init->init_array_full;
			ASSERT(array_type->type_kind == TYPE_ARRAY || array_type->type_kind == TYPE_VECTOR);
			ArraySize size = array_type->array.len;
			ASSERT(size > 0);
			LLVMValueRef *parts = VECNEW(LLVMValueRef, size);
			for (ArrayIndex i = 0; i < (ArrayIndex)size; i++)
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
			ASSERT(const_init->type->type_kind != TYPE_SLICE);
			bool was_modified = false;
			Type *array_type = const_init->type;
			Type *element_type = array_type->array.base;
			LLVMTypeRef element_type_llvm = llvm_get_type(c, element_type);
			AlignSize expected_align = llvm_abi_alignment(c, element_type_llvm);
			ConstInitializer **elements = const_init->init_array.elements;
			ASSERT(vec_size(elements) > 0 && "Array should always have gotten at least one element.");
			ArrayIndex current_index = 0;
			unsigned alignment = 0;
			LLVMValueRef *parts = NULL;
			bool pack = false;
			FOREACH(ConstInitializer *, element, elements)
			{
				ASSERT(element->kind == CONST_INIT_ARRAY_VALUE);
				ArrayIndex element_index = element->init_array_value.index;
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

			IndexDiff end_diff = (ArrayIndex)array_type->array.len - current_index;
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
			for (ArrayIndex i = 0; i < count; i++)
			{
				if (members[i]->padding)
				{
					vec_add(entries, llvm_emit_const_padding(c, members[i]->padding));
				}
				LLVMTypeRef expected_type = llvm_get_type(c, const_init->init_struct[i]->type);
				LLVMValueRef element = llvm_emit_const_initializer(c, const_init->init_struct[i]);
				LLVMTypeRef element_type = LLVMTypeOf(element);
				//ASSERT(LLVMIsConstant(element));
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
			llvm_emit_expr_global_value(c, &value, const_init->init_value);
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
		case TYPE_SLICE:
		{
			BEValue member;
			llvm_emit_slice_pointer(c, value, &member);
			llvm_value_rvalue(c, &member);
			llvm_value_set_address(c,
			                       value,
			                       member.value,
			                       type_get_ptr(value->type->array.base), type_abi_alignment(value->type->array.base));
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

void llvm_set_weak(GenContext *c, LLVMValueRef global)
{
	LLVMSetLinkage(global, compiler.platform.os == OS_TYPE_WIN32 ? LLVMWeakODRLinkage : LLVMWeakAnyLinkage);
	LLVMSetVisibility(global, LLVMDefaultVisibility);
	llvm_set_comdat(c, global);
}

static void llvm_set_external_reference(LLVMValueRef ref, bool is_weak)
{
	if (compiler.platform.os == OS_TYPE_WIN32) is_weak = false;
	LLVMSetLinkage(ref, is_weak ? LLVMExternalWeakLinkage : LLVMExternalLinkage);
	LLVMSetVisibility(ref, LLVMDefaultVisibility);
}

void llvm_set_decl_linkage(GenContext *c, Decl *decl)
{
	bool is_var = decl->decl_kind == DECL_VAR;
	bool is_weak = decl->is_weak;
	bool should_weaken = is_weak || (!decl->is_extern && module_should_weaken(decl->unit->module));
	LLVMValueRef ref = decl->backend_ref;
	LLVMValueRef opt_ref = is_var ? decl->var.optional_ref : NULL;
	bool is_static = is_var && decl->var.is_static;
	// Static variables in a different modules should be copied to the current module.
	bool same_module = is_static || decl->unit->module == c->code_module;
	if (decl->is_extern || !same_module)
	{
		llvm_set_external_reference(ref, should_weaken);
		if (opt_ref) llvm_set_external_reference(opt_ref, should_weaken);
		return;
	}
	if (decl_is_externally_visible(decl) && !is_static)
	{
		if (decl->is_export && compiler.platform.os == OS_TYPE_WIN32  && !compiler.build.win.def && decl->name != kw_main && decl->name != kw_mainstub)
		{
			LLVMSetDLLStorageClass(ref, LLVMDLLExportStorageClass);
		}
		if (should_weaken)
		{
			llvm_set_weak(c, ref);
			if (opt_ref) llvm_set_weak(c, opt_ref);
			return;
		}
		LLVMSetVisibility(ref, LLVMDefaultVisibility);
		if (opt_ref) LLVMSetVisibility(opt_ref, LLVMDefaultVisibility);
		return;
	}

	LLVMSetLinkage(ref, decl->is_weak ? LLVMLinkerPrivateWeakLinkage : LLVMInternalLinkage);
	if (opt_ref) LLVMSetLinkage(opt_ref, LLVMInternalLinkage);
}

void llvm_set_internal_linkage(LLVMValueRef alloc)
{
	LLVMSetLinkage(alloc, LLVMInternalLinkage);
	LLVMSetVisibility(alloc, LLVMDefaultVisibility);
}

void llvm_set_private_declaration(LLVMValueRef alloc)
{
	LLVMSetLinkage(alloc, LLVMPrivateLinkage);
	LLVMSetVisibility(alloc, LLVMDefaultVisibility);
	LLVMSetUnnamedAddress(alloc, LLVMGlobalUnnamedAddr);
}

void llvm_emit_global_variable_init(GenContext *c, Decl *decl)
{
	ASSERT(decl->var.kind == VARDECL_GLOBAL || decl->var.kind == VARDECL_CONST || decl->var.is_static);

	// Skip real constants.
	if (!decl->type) return;

	decl_append_links_to_global(decl);

	LLVMValueRef init_value;

	Type *var_type = type_lowering(decl->type);

	Expr *init_expr = decl->var.init_expr;
	// Fold "source" of the init.
	while (init_expr && init_expr->expr_kind == EXPR_IDENTIFIER)
	{
		Decl *inner_decl = decl_flatten(init_expr->ident_expr);
		if (inner_decl->decl_kind != DECL_VAR) break;
		if (inner_decl->var.kind != VARDECL_CONST) break;
		init_expr = inner_decl->var.init_expr;
	}
	if (init_expr && init_expr->expr_kind != EXPR_OPTIONAL)
	{
		if (expr_is_const_initializer(init_expr))
		{
			ASSERT(type_flatten(init_expr->type)->type_kind != TYPE_SLICE);
			ConstInitializer *list = init_expr->const_expr.initializer;
			init_value = llvm_emit_const_initializer(c, list);
		}
		else
		{
			BEValue value;
			llvm_emit_expr_global_value(c, &value, init_expr);
			init_value = llvm_load_value_store(c, &value);
		}
	}
	else
	{
		init_value = decl->var.no_init ? llvm_get_undef(c, var_type) : llvm_get_zero(c, var_type);
	}


	LLVMValueRef old = decl->backend_ref;
	scratch_buffer_set_extern_decl_name(decl, true);
	char *name = scratch_buffer_copy();
	LLVMValueRef global_ref = decl->backend_ref = llvm_add_global_raw(c, name, LLVMTypeOf(init_value), decl->alignment);
	if (llvm_use_debug(c))
	{
		SourceSpan loc = decl->span;
		decl->var.backend_debug_ref = LLVMDIBuilderCreateGlobalVariableExpression(
				c->debug.builder,
				c->debug.file.debug_file,
				decl->name,
				strlen(decl->name),
				name,
				strlen(name),
				c->debug.file.debug_file,
				loc.row ? loc.row : 1,
				llvm_get_debug_type(c, decl->type),
				decl_is_local(decl),
				LLVMDIBuilderCreateExpression(c->debug.builder, NULL, 0),
				NULL,
				decl->alignment);
		LLVMGlobalSetMetadata(llvm_get_ref(c, decl), 0, decl->var.backend_debug_ref);
	}

	if (decl->var.is_addr)
	{
		LLVMSetUnnamedAddress(global_ref, LLVMNoUnnamedAddr);
	}
	else
	{
		LLVMSetUnnamedAddress(decl->backend_ref,
							  decl_is_local(decl) ? LLVMGlobalUnnamedAddr : LLVMLocalUnnamedAddr);
	}
	if (decl->attrs_resolved && decl->attrs_resolved->section)
	{
		LLVMSetSection(global_ref, decl->attrs_resolved->section);
	}
	llvm_set_global_tls(decl);

	LLVMValueRef optional_ref = decl->var.optional_ref;
	if (optional_ref)
	{
		llvm_set_alignment(optional_ref, type_alloca_alignment(type_fault));
		LLVMSetUnnamedAddress(optional_ref, LLVMGlobalUnnamedAddr);
	}
	LLVMValueRef optional_value = NULL;
	if (init_expr && IS_OPTIONAL(init_expr) && init_expr->expr_kind == EXPR_OPTIONAL)
	{
		Expr *inner = init_expr->inner_expr;
		ASSERT(expr_is_const(inner) && inner->const_expr.const_kind == CONST_FAULT);
		BEValue value;
		llvm_emit_expr_global_value(c, &value, inner);
		optional_value = llvm_load_value_store(c, &value);
	}
	if (!decl->is_extern)
	{
		LLVMSetInitializer(decl->backend_ref, init_value);
		if (optional_ref)
		{
			LLVMSetInitializer(optional_ref, optional_value ? optional_value : llvm_get_zero(c, type_fault));
		}
	}

	LLVMSetGlobalConstant(global_ref, decl->var.kind == VARDECL_CONST);

	decl->backend_ref = global_ref;
	if (old)
	{
		LLVMReplaceAllUsesWith(old, global_ref);
		LLVMDeleteGlobal(old);
	}

	llvm_set_decl_linkage(c, decl);

}
static void gencontext_verify_ir(GenContext *context)
{
	char *error = NULL;
	ASSERT(context->module);
	if (LLVMVerifyModule(context->module, LLVMPrintMessageAction, &error))
	{
		if (*error)
		{
			eprintf("----------------------------------IR integrity failure:\n");
			LLVMDumpModule(context->module);
			error_exit("Could not verify IR: %s", error);
		}
		error_exit("Could not verify module IR.");
	}
}

static void llvm_emit_file(GenContext *c, const char *filename, LLVMCodeGenFileType llvm_codegen_type, bool clone_module)
{
	DEBUG_LOG("Target: %s", compiler.platform.target_triple);
	LLVMModuleRef module = clone_module ? LLVMCloneModule(c->module) : c->module;
	LLVMSetTarget(module, compiler.platform.target_triple);
	char *layout = LLVMCopyStringRepOfTargetData(c->target_data);
	LLVMSetDataLayout(module, layout);
	LLVMDisposeMessage(layout);

	char *err = "";
	FILE *file = NULL;
	LLVMMemoryBufferRef buffer = NULL;
	if (LLVMTargetMachineEmitToMemoryBuffer(c->machine, module, llvm_codegen_type, &err, &buffer))
	{
		goto ERR;
	}
	if (clone_module)
	{
		LLVMDisposeModule(module);
	}
	file = fopen(filename, "wb");
	if (!file)
	{
		err = "File could not be opened";
		goto ERR;
	}
	size_t len = LLVMGetBufferSize(buffer);
	const char *ptr = LLVMGetBufferStart(buffer);
	while (len > 0)
	{
		size_t written = fwrite(ptr, 1, len, file);
		if (written == 0)
		{
			err = "Failed to write to file";
			goto ERR;
		}
		ptr += written;
		len -= written;
	}
	fclose(file);
	LLVMDisposeMemoryBuffer(buffer);
	return;
ERR:
	if (file) fclose(file);
	if (buffer) LLVMDisposeMemoryBuffer(buffer);
	error_exit("Could not emit '%s': %s", filename, err);
}

void gencontext_print_llvm_ir(GenContext *context)
{
	char *err = NULL;
	if (LLVMPrintModuleToFile(context->module, context->ir_filename, &err))
	{
		error_exit("Could not emit ir '%s' to file: %s", context->ir_filename, err);
	}
}


LLVMValueRef llvm_emit_alloca(GenContext *c, LLVMTypeRef type, unsigned alignment, const char *name)
{
	ASSERT(LLVMGetTypeKind(type) != LLVMVoidTypeKind);
	ASSERT(!llvm_is_global_eval(c));
	ASSERT(alignment > 0);
	LLVMBasicBlockRef current_block = LLVMGetInsertBlock(c->builder);
	LLVMPositionBuilderBefore(c->builder, c->alloca_point);
	ASSERT(LLVMGetTypeContext(type) == c->context);
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
	ASSERT(!decl->backend_ref && !decl->is_value);
	decl->backend_ref = llvm_emit_alloca(c, llvm_get_type(c, type), decl->alignment, decl->name ? decl->name : ".anon");
}

void llvm_emit_local_var_alloca(GenContext *c, Decl *decl)
{
	ASSERT(!decl->var.is_static && decl->var.kind != VARDECL_CONST);
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
	intrinsic_id.debugtrap = lookup_intrinsic("llvm.debugtrap");
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
	intrinsic_id.matrix_multiply = lookup_intrinsic("llvm.matrix.multiply");
	intrinsic_id.matrix_transpose = lookup_intrinsic("llvm.matrix.transpose");
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
	intrinsic_id.smul_fixed_sat = lookup_intrinsic("llvm.smul.fix.sat");
	intrinsic_id.threadlocal_address = lookup_intrinsic("llvm.threadlocal.address");
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
	intrinsic_id.umul_fixed_sat = lookup_intrinsic("llvm.umul.fix.sat");
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
	attribute_id.sanitize_address = lookup_attribute("sanitize_address");
	attribute_id.sanitize_memory = lookup_attribute("sanitize_memory");
	attribute_id.sanitize_thread = lookup_attribute("sanitize_thread");
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
	if (!compiler.platform.use_comdat) return;
	LLVMComdatRef comdat = LLVMGetOrInsertComdat(c->module, LLVMGetValueName(global));
	LLVMSetComdatSelectionKind(comdat, LLVMAnyComdatSelectionKind);
	LLVMSetComdat(global, comdat);
}

void llvm_set_selector_linkage(GenContext *c, LLVMValueRef selector)
{
	LLVMSetVisibility(selector, LLVMDefaultVisibility);
	LLVMSetLinkage(selector, LLVMLinkOnceODRLinkage);
	llvm_set_comdat(c, selector);
}

void llvm_set_linkonce(GenContext *c, LLVMValueRef global)
{
	LLVMSetLinkage(global, LLVMLinkOnceAnyLinkage);
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

LLVMBuilderRef llvm_create_function_entry(GenContext *c, LLVMValueRef func, LLVMBasicBlockRef *entry_block_ref)
{
	LLVMBasicBlockRef entry = llvm_append_basic_block(c, func, "entry");
	LLVMBuilderRef builder = llvm_create_builder(c);
	LLVMPositionBuilderAtEnd(builder, entry);
	if (entry_block_ref) *entry_block_ref = entry;
	return builder;
}

LLVMBasicBlockRef llvm_append_basic_block(GenContext *c, LLVMValueRef func, const char *name)
{
	return LLVMAppendBasicBlockInContext(c->context, func, name);
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
			if (decl->type_alias_decl.is_func)
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
		case DECL_BITSTRUCT:
		case DECL_CONST_ENUM:
			llvm_get_typeid(context, decl->type);
			break;
	}
}


static inline void llvm_optimize(GenContext *c)
{
	bool should_debug = false;
#ifndef NDEBUG
	should_debug = debug_log;
#endif
	LLVMOptLevels level = LLVM_O0;

	switch (compiler.build.optsize)
	{
		case SIZE_OPTIMIZATION_SMALL:
			level = LLVM_Os;
			break;
		case SIZE_OPTIMIZATION_TINY:
			level = LLVM_Oz;
			break;
		default:
			switch (compiler.build.optlevel)
			{
				case OPTIMIZATION_NONE:
				case OPTIMIZATION_NOT_SET:
					level = LLVM_O0;
					break;
				case OPTIMIZATION_LESS:
					level = LLVM_O1;
					break;
				case OPTIMIZATION_MORE:
					level = LLVM_O2;
					break;
				case OPTIMIZATION_AGGRESSIVE:
					level = LLVM_O3;
					break;
			}
			break;
	}
	LLVMPasses passes = {
			.opt_level = level,
			.should_verify = compiler.build.emit_llvm,
			.should_debug = should_debug,
			.is_kernel = compiler.build.kernel_build,
			.opt.vectorize_loops = compiler.build.loop_vectorization == VECTORIZATION_ON,
			.opt.slp_vectorize = compiler.build.slp_vectorization == VECTORIZATION_ON,
			.opt.unroll_loops = compiler.build.unroll_loops == UNROLL_LOOPS_ON,
			.opt.interleave_loops = compiler.build.unroll_loops == UNROLL_LOOPS_ON,
			.opt.merge_functions = compiler.build.merge_functions == MERGE_FUNCTIONS_ON,
			.sanitizer.address_sanitize = compiler.build.feature.sanitize_address,
			.sanitizer.mem_sanitize = compiler.build.feature.sanitize_memory,
			.sanitizer.thread_sanitize = compiler.build.feature.sanitize_thread
	};
	if (!llvm_run_passes(c->module, c->machine, &passes))
	{
		error_exit("Failed to run passes.");
	}
}

const char *llvm_codegen(void *context)
{
	GenContext *c = context;
	if (!compiler_should_ouput_file(c->base_name)) return NULL;
	llvm_optimize(c);

	// Serialize the LLVM IR, if requested, also verify the IR in this case
	if (compiler.build.emit_llvm)
	{
		gencontext_print_llvm_ir(c);
		gencontext_verify_ir(c);
	}

	const char *object_name = NULL;
	if (compiler.build.emit_asm)
	{
		// Clone if there will be object file output.
		llvm_emit_file(c, c->asm_filename, LLVMAssemblyFile, compiler.build.emit_object_files);
	}

	if (compiler.build.emit_object_files)
	{
		llvm_emit_file(c, c->object_filename, LLVMObjectFile, false);
		object_name = c->object_filename;
	}

	gencontext_end_module(c);
	gencontext_destroy(c);

	return object_name;
}


void llvm_add_global_decl(GenContext *c, Decl *decl)
{
	ASSERT_SPAN(decl, decl->var.kind == VARDECL_GLOBAL || decl->var.kind == VARDECL_CONST);

	bool same_module = decl->unit->module == c->code_module;
	LLVMTypeRef type = llvm_get_type(c, decl->type);
	if (same_module)
	{
		// If we initialize it later, we must use a temp name.
		decl->backend_ref = llvm_add_global_raw(c, ".tempglobal", type, decl->alignment);
	}
	else
	{
		scratch_buffer_set_extern_decl_name(decl, true);
		decl->backend_ref = llvm_add_global_raw(c, scratch_buffer_to_string(), type, decl->alignment);
	}
	if (decl->var.kind == VARDECL_CONST)
	{
		LLVMSetGlobalConstant(decl->backend_ref, true);
	}
	if (IS_OPTIONAL(decl))
	{
		LLVMTypeRef fault = llvm_get_type(c, type_fault);
		scratch_buffer_set_extern_decl_name(decl, true);
		scratch_buffer_append(".f");
		decl->var.optional_ref = llvm_add_global_raw(c, scratch_buffer_to_string(), fault, 0);
	}
	llvm_set_decl_linkage(c, decl);
	llvm_set_global_tls(decl);
}

LLVMValueRef llvm_get_opt_ref(GenContext *c, Decl *decl)
{
	llvm_get_ref(c, decl);
	decl = decl_flatten(decl);
	if (decl->decl_kind != DECL_VAR) return NULL;
	return decl->var.optional_ref;
}

static void llvm_emit_param_attributes(GenContext *c, LLVMValueRef function, ABIArgInfo *info, bool is_return,
                                       int index, int last_index, Decl *decl)
{
	ASSERT(last_index == index || info->kind == ABI_ARG_DIRECT_PAIR || info->kind == ABI_ARG_IGNORE
	       || info->kind == ABI_ARG_EXPAND || info->kind == ABI_ARG_DIRECT || info->kind == ABI_ARG_DIRECT_COERCE
	       || info->kind == ABI_ARG_DIRECT_COERCE_INT || info->kind == ABI_ARG_EXPAND_COERCE
	       || info->kind == ABI_ARG_DIRECT_SPLIT_STRUCT_I32);

	if (info->attributes.zeroext)
	{
		// Direct only
		ASSERT(index == last_index);
		llvm_attribute_add(c, function, attribute_id.zext, index);
	}
	if (info->attributes.signext)
	{
		// Direct only
		ASSERT(index == last_index);
		llvm_attribute_add(c, function, attribute_id.sext, index);
	}
	if (info->attributes.by_reg)
	{
		llvm_attribute_add_range(c, function, attribute_id.inreg, index, last_index);
	}
	switch (info->kind)
	{
		case ABI_ARG_DIRECT:
			if (decl && decl->var.no_alias)
			{
				llvm_attribute_add(c, function, attribute_id.noalias, 1);
			}
			break;
		case ABI_ARG_EXPAND:
		case ABI_ARG_IGNORE:
		case ABI_ARG_DIRECT_SPLIT_STRUCT_I32:
		case ABI_ARG_DIRECT_COERCE:
		case ABI_ARG_DIRECT_COERCE_INT:
		case ABI_ARG_DIRECT_PAIR:
		case ABI_ARG_EXPAND_COERCE:
			break;
		case ABI_ARG_INDIRECT:
			if (is_return)
			{
				ASSERT(info->indirect.type);
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
	llvm_emit_param_attributes(c, function, ret_abi_info, true, 0, 0, NULL);
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
		llvm_emit_param_attributes(c, function, prototype->ret_by_ref_abi_info, false, info->param_index_start + 1,
		                           info->param_index_end, NULL);
	}
	for (unsigned i = 0; i < params; i++)
	{
		ABIArgInfo *info = prototype->abi_args[i];
		llvm_emit_param_attributes(c, function, info, false, info->param_index_start + 1, info->param_index_end, decl->func_decl.signature.params[i]);
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
	if (decl->is_export && arch_is_wasm(compiler.platform.arch))
	{
		if (c->code_module == decl->unit->module)
		{
			scratch_buffer_set_extern_decl_name(decl, true);
			llvm_attribute_add_string(c, function, "wasm-export-name", scratch_buffer_to_string(), -1);
		}
	}
	if (decl->is_extern && arch_is_wasm(compiler.platform.arch))
	{
		scratch_buffer_set_extern_decl_name(decl, true);
		llvm_attribute_add_string(c, function, "wasm-import-name", scratch_buffer_to_string(), -1);
		if (decl->attrs_resolved && decl->attrs_resolved->wasm_module)
		{
			llvm_attribute_add_string(c, function, "wasm-import-module", decl->attrs_resolved->wasm_module, -1);
		}
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

	if (compiler.build.feature.sanitize_address && !decl->func_decl.attr_nosanitize_address)
	{
		llvm_attribute_add(c, function, attribute_id.sanitize_address, -1);
	}
	if (compiler.build.feature.sanitize_memory && !decl->func_decl.attr_nosanitize_memory)
	{
		llvm_attribute_add(c, function, attribute_id.sanitize_memory, -1);
	}
	if (compiler.build.feature.sanitize_thread && !decl->func_decl.attr_nosanitize_thread)
	{
		llvm_attribute_add(c, function, attribute_id.sanitize_thread, -1);
	}

	LLVMSetFunctionCallConv(function, llvm_call_convention_from_call(prototype->call_abi));
}
static LLVMValueRef llvm_create_fault(GenContext *c, Decl *decl)
{
	scratch_buffer_set_extern_decl_name(decl, true);
	LLVMValueRef fault = llvm_add_global_raw(c, scratch_buffer_to_string(), c->chars_type, 0);
	LLVMSetGlobalConstant(fault, 1);
	scratch_buffer_append(".nameof");
	if (decl->obfuscate)
	{
		LLVMSetInitializer(fault, llvm_emit_string_const(c, "<FAULT>", scratch_buffer_to_string()));
	}
	else
	{
		const char *module_name = decl->unit->module->name->module;
		size_t last = 0;
		for (size_t i = 0;; i++)
		{
			if (module_name[i] == 0) break;
			if (module_name[i] == ':')
			{
				i++;
				last = i + 1;
			}
		}
		const char *new_name = str_printf("%s::%s", &module_name[last], decl->name);
		LLVMSetInitializer(fault, llvm_emit_string_const(c, new_name, scratch_buffer_to_string()));
	}
	llvm_set_linkonce(c, fault);
	decl->backend_ref = fault;
	return fault;
}

LLVMValueRef llvm_get_ref(GenContext *c, Decl *decl)
{
	ASSERT(!decl->is_value);
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
			ASSERT_SPAN(decl, decl->var.kind == VARDECL_GLOBAL || decl->var.kind == VARDECL_CONST);
			llvm_add_global_decl(c, decl);
			return decl->backend_ref;
		case DECL_FUNC:
			if (decl->func_decl.attr_interface_method)
			{
				return decl->backend_ref = llvm_get_selector(c, decl->name);
			}
			LLVMTypeRef type = llvm_get_type(c, decl->type);
			scratch_buffer_set_extern_decl_name(decl, true);
			if (decl->name == kw_memcmp && c->memcmp_function)
			{
				backend_ref = decl->backend_ref = c->memcmp_function;
			}
			else
			{
				const char *name = scratch_buffer_to_string();
				if (decl->is_extern && (backend_ref = LLVMGetNamedFunction(c->module, name))) // NOLINT
				{
					return decl->backend_ref = backend_ref;
				}
				backend_ref = decl->backend_ref = LLVMAddFunction(c->module, name, type);
			}
			llvm_append_function_attributes(c, decl);
			llvm_set_decl_linkage(c, decl);
			return backend_ref;
		case DECL_ALIAS:
			return llvm_get_ref(c, decl->define_decl.alias);
		case DECL_FAULT:
			return llvm_create_fault(c, decl);
		case DECL_POISONED:
		case DECL_ATTRIBUTE:
		case DECL_BITSTRUCT:
		case DECL_CT_ASSERT:
		case DECL_DISTINCT:
		case DECL_ENUM:
		case DECL_CONST_ENUM:
		case DECL_ENUM_CONSTANT:
		case DECL_IMPORT:
		case DECL_ALIAS_PATH:
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
		case DECL_GROUP:
		case DECL_INTERFACE:
			UNREACHABLE;
	}
	UNREACHABLE
}


INLINE GenContext *llvm_gen_tests(Module** modules, unsigned module_count, LLVMContextRef shared_context)
{
	Path *test_path = path_create_from_string("_$test", 5, INVALID_SPAN);
	Module *test_module = compiler_find_or_create_module(test_path, NULL);

	DebugInfo actual_debug_info = compiler.build.debug_info;
	compiler.build.debug_info = DEBUG_INFO_NONE;

	GenContext *c = cmalloc(sizeof(GenContext));
	gencontext_init(c, test_module, shared_context);
	gencontext_begin_module(c);

	LLVMValueRef *names = NULL;
	LLVMValueRef *decls = NULL;

	LLVMTypeRef opt_test = LLVMFunctionType(llvm_get_type(c, type_fault), NULL, 0, false);
	for (unsigned i = 0; i < module_count; i++)
	{
		Module *module = modules[i];
		FOREACH(Decl *, test, module->tests)
		{
			LLVMTypeRef type = opt_test;
			scratch_buffer_set_extern_decl_name(test, true);
			LLVMValueRef ref = LLVMAddFunction(c->module, scratch_buffer_to_string(), type);
			scratch_buffer_clear();
			scratch_buffer_printf("%s::%s", module->name->module, test->name);
			LLVMValueRef name = llvm_emit_string_const(c, scratch_buffer_to_string(), ".test.name");
			vec_add(names, name);
			vec_add(decls, ref);
		}
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
	Type *chars_array = type_get_slice(type_chars);
	LLVMValueRef name_list = llvm_add_global(c, test_names_var_name, chars_array, type_alloca_alignment(chars_array));
	LLVMSetGlobalConstant(name_list, 1);
	LLVMSetInitializer(name_list, llvm_emit_aggregate_two(c, chars_array, name_ref, count));
	Type *decls_array_type = type_get_slice(type_voidptr);
	LLVMValueRef decl_list = llvm_add_global(c, test_fns_var_name, decls_array_type, type_alloca_alignment(decls_array_type));
	LLVMSetGlobalConstant(decl_list, 1);
	LLVMSetInitializer(decl_list, llvm_emit_aggregate_two(c, decls_array_type, decl_ref, count));

	compiler.build.debug_info = actual_debug_info;
	return c;
}


INLINE GenContext *llvm_gen_benchmarks(Module** modules, unsigned module_count, LLVMContextRef shared_context)
{
	Path *benchmark_path = path_create_from_string("$benchmark", 10, INVALID_SPAN);
	Module *benchmark_module = compiler_find_or_create_module(benchmark_path, NULL);

	DebugInfo actual_debug_info = compiler.build.debug_info;
	compiler.build.debug_info = DEBUG_INFO_NONE;
	GenContext *c = cmalloc(sizeof(GenContext));
	gencontext_init(c, benchmark_module, shared_context);
	gencontext_begin_module(c);

	LLVMValueRef *names = NULL;
	LLVMValueRef *decls = NULL;

	LLVMTypeRef opt_benchmark = LLVMFunctionType(llvm_get_type(c, type_fault), NULL, 0, false);
	for (unsigned i = 0; i < module_count; i++)
	{
		Module *module = modules[i];
		FOREACH(Decl *, benchmark, module->benchmarks)
		{
			LLVMTypeRef type = opt_benchmark;
			scratch_buffer_set_extern_decl_name(benchmark, true);
			LLVMValueRef ref = LLVMAddFunction(c->module, scratch_buffer_to_string(), type);
			scratch_buffer_clear();
			scratch_buffer_printf("%s::%s", module->name->module, benchmark->name);
			LLVMValueRef name = llvm_emit_string_const(c, scratch_buffer_to_string(), ".benchmark.name");
			vec_add(names, name);
			vec_add(decls, ref);
		}
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
	Type *chars_array = type_get_slice(type_chars);
	LLVMValueRef name_list = llvm_add_global(c, benchmark_names_var_name, chars_array, type_alloca_alignment(chars_array));
	LLVMSetGlobalConstant(name_list, 1);
	LLVMSetInitializer(name_list, llvm_emit_aggregate_two(c, chars_array, name_ref, count));
	Type *decls_array_type = type_get_slice(type_voidptr);
	LLVMValueRef decl_list = llvm_add_global(c, benchmark_fns_var_name, decls_array_type, type_alloca_alignment(decls_array_type));
	LLVMSetGlobalConstant(decl_list, 1);
	LLVMSetInitializer(decl_list, llvm_emit_aggregate_two(c, decls_array_type, decl_ref, count));

	compiler.build.debug_info = actual_debug_info;
	return c;
}

void **llvm_gen(Module** modules, unsigned module_count)
{
	if (!module_count) return NULL;
	GenContext **gen_contexts = NULL;
	llvm_codegen_setup();
	if (compiler.build.single_module == SINGLE_MODULE_ON)
	{
		LLVMContextRef context = LLVMGetGlobalContext();
		for (int i = 0; i < module_count; i++)
		{
			GenContext *result = llvm_gen_module(modules[i], context);
			if (!result) continue;
			vec_add(gen_contexts, result);
		}
		if (!gen_contexts) return NULL;
		GenContext *first = gen_contexts[0];
		if (compiler.build.benchmarking)
		{
			vec_add(gen_contexts, llvm_gen_benchmarks(modules, module_count, context));
		}
		if (compiler.build.testing)
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
	if (compiler.build.benchmarking)
	{
		vec_add(gen_contexts, llvm_gen_benchmarks(modules, module_count, NULL));
	}
	if (compiler.build.testing)
	{
		vec_add(gen_contexts, llvm_gen_tests(modules, module_count, NULL));
	}
	return (void**)gen_contexts;
}

LLVMMetadataRef llvm_get_debug_file(GenContext *c, FileId file_id)
{
	FOREACH(DebugFile, file, c->debug.debug_files)
	{
		if (file.file_id == file_id) return file.debug_file; // NOLINT
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
	if (compiler.build.emit_stdlib == EMIT_STDLIB_OFF && module_is_stdlib(module)) return NULL;

	ASSERT(intrinsics_setup);

	bool has_elements = false;
	GenContext *gen_context = cmalloc(sizeof(GenContext));
	gencontext_init(gen_context, module, shared_context);
	gencontext_begin_module(gen_context);

	bool only_used = strip_unused();

	FOREACH(CompilationUnit *, unit, module->units)
	{
		gencontext_init_file_emit(gen_context, unit);
		gen_context->debug.compile_unit = unit->llvm.debug_compile_unit;
		gen_context->debug.file = (DebugFile){ .debug_file = unit->llvm.debug_file, .file_id = unit->file->file_id };

		FOREACH(Decl *, method, unit->methods)
		{
			if (only_used && !method->is_live) continue;
			llvm_emit_function_decl(gen_context, method);
		}

		FOREACH(Decl *, type_decl, unit->types)
		{
			if (only_used && !type_decl->is_live) continue;
			llvm_emit_type_decls(gen_context, type_decl);
		}

		FOREACH(Decl *, enum_decl, unit->enums)
		{
			if (only_used && !enum_decl->is_live) continue;
			llvm_emit_type_decls(gen_context, enum_decl);
		}

		FOREACH(Decl *, func, unit->functions)
		{
			if (only_used && !func->is_live) continue;
			if (func->func_decl.attr_test)
			{
				if (!compiler.build.testing) continue;
				vec_add(module->tests, func);
			}
			if (func->func_decl.attr_benchmark)
			{
				if (!compiler.build.benchmarking) continue;
				vec_add(module->benchmarks, func);
			}
			llvm_emit_function_decl(gen_context, func);
		}


		FOREACH(Decl *, func, unit->lambdas)
		{
			if (only_used && !func->is_live) continue;
			has_elements = true;
			llvm_emit_function_decl(gen_context, func);
		}

		if (unit->main_function && unit->main_function->is_synthetic)
		{
			has_elements = true;
			llvm_emit_function_decl(gen_context, unit->main_function);
		}

	}

	FOREACH(CompilationUnit *, unit, module->units)
	{
		gen_context->debug.compile_unit = unit->llvm.debug_compile_unit;
		gen_context->debug.file = (DebugFile){
				.debug_file = unit->llvm.debug_file,
				.file_id = unit->file->file_id };

		FOREACH(Decl *, var, unit->vars)
		{
			if (only_used && !var->is_live) continue;
			has_elements = true;
			llvm_get_ref(gen_context, var);
		}

		FOREACH(Decl *, var, unit->vars)
		{
			if (only_used && !var->is_live) continue;
			has_elements = true;
			llvm_emit_global_variable_init(gen_context, var);
		}

		FOREACH(Decl *, decl, unit->functions)
		{
			if (decl->func_decl.attr_test && !compiler.build.testing) continue;
			if (decl->func_decl.attr_benchmark && !compiler.build.benchmarking) continue;
			if (only_used && !decl->is_live) continue;
			if (decl->func_decl.body)
			{
				has_elements = true;
				llvm_emit_function_body(gen_context, decl);
			}
		}

		FOREACH(Decl *, func, unit->lambdas)
		{
			if (only_used && !func->is_live) continue;
			has_elements = true;
			llvm_emit_function_body(gen_context, func);
		}

		if (unit->main_function && unit->main_function->is_synthetic)
		{
			has_elements = true;
			llvm_emit_function_body(gen_context, unit->main_function);
		}

		FOREACH(Decl *, decl, unit->methods)
		{
			if (only_used && !decl->is_live) continue;
			if (!decl->func_decl.body) continue;
			has_elements = true;
			llvm_emit_function_body(gen_context, decl);
		}

		gencontext_end_file_emit(gen_context, unit);

	}

	llvm_emit_dynamic_functions(gen_context, gen_context->dynamic_functions);

	llvm_emit_constructors_and_destructors(gen_context);

	if (llvm_use_debug(gen_context))
	{
		LLVMDIBuilderFinalize(gen_context->debug.builder);
		LLVMDisposeDIBuilder(gen_context->debug.builder);
	}

	// If it's in test or benchmark, then we want to serialize the IR before it is optimized.
	if (compiler.build.test_output || compiler.build.benchmark_output)
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

void llvm_attribute_add(GenContext *c, LLVMValueRef value_to_add_attribute_to, unsigned attribute, int index)
{
	llvm_attribute_add_int(c, value_to_add_attribute_to, attribute, 0, index);
}

void llvm_attribute_add_call_type(GenContext *c, LLVMValueRef call, unsigned attribute, int index, LLVMTypeRef type)
{
	LLVMAttributeRef llvm_attr = LLVMCreateTypeAttribute(c->context, attribute, type);
	LLVMAddCallSiteAttribute(call, (LLVMAttributeIndex)index, llvm_attr);
}

void llvm_attribute_add_call(GenContext *c, LLVMValueRef call, unsigned attribute, int index, int64_t value)
{
	LLVMAttributeRef llvm_attr = LLVMCreateEnumAttribute(c->context, attribute, (uint64_t)value);
	LLVMAddCallSiteAttribute(call, (LLVMAttributeIndex)index, llvm_attr);
}

void llvm_attribute_add_range(GenContext *c, LLVMValueRef value_to_add_attribute_to, unsigned attribute, int index_start, int index_end)
{
	for (int i = index_start; i <= index_end; i++)
	{
		llvm_attribute_add_int(c, value_to_add_attribute_to, attribute, 0, i);
	}
}

void llvm_attribute_add_string(GenContext *c, LLVMValueRef value_to_add_attribute_to, const char *attribute, const char *value, int index)
{
	LLVMAttributeRef llvm_attr = LLVMCreateStringAttribute(c->context, attribute, (unsigned)strlen(attribute), value, (unsigned)strlen(value));
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
	ASSERT(dest_align && src_align);
	if (len <= UINT32_MAX)
	{
		return LLVMBuildMemCpy(c->builder, dest, dest_align, source, src_align, llvm_const_int(c, type_uint, len));
	}
	return LLVMBuildMemCpy(c->builder, dest, dest_align, source, src_align, llvm_const_int(c, type_ulong, len));
}

TypeSize llvm_store_size(GenContext *c, LLVMTypeRef type)
{
	return (TypeSize)LLVMStoreSizeOfType(c->target_data, type);
}

TypeSize llvm_alloc_size(GenContext *c, LLVMTypeRef type)
{
	return (TypeSize)aligned_offset((AlignSize)LLVMABISizeOfType(c->target_data, type), llvm_abi_alignment(c, type));
}

