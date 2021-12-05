// Copyright (c) 2019 Christoffer Lerno. All rights reserved.
// Use of this source code is governed by the GNU LGPLv3.0 license
// a copy of which can be found in the LICENSE file.

#include "llvm_codegen_internal.h"

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

static void gencontext_init(GenContext *context, Module *module)
{
	memset(context, 0, sizeof(GenContext));
	context->context = LLVMContextCreate();
	context->bool_type = LLVMInt1TypeInContext(context->context);
	context->byte_type = LLVMInt8TypeInContext(context->context);
	LLVMContextSetDiagnosticHandler(context->context, &diagnostics_handler, context);
	context->code_module = module;
}

static void gencontext_destroy(GenContext *context)
{
	LLVMContextDispose(context->context);
	LLVMDisposeTargetData(context->target_data);
	LLVMDisposeTargetMachine(context->machine);
	free(context);
}

LLVMValueRef llvm_emit_memclear_size_align(GenContext *c, LLVMValueRef ref, uint64_t size, AlignSize align, bool bitcast)
{

	LLVMValueRef target = bitcast ? LLVMBuildBitCast(c->builder, ref, llvm_get_type(c, type_get_ptr(type_char)), "") : ref;
	return LLVMBuildMemSet(c->builder, target, LLVMConstInt(llvm_get_type(c, type_char), 0, false),
	                       LLVMConstInt(llvm_get_type(c, type_ulong), size, false), align);

}

void llvm_emit_memclear(GenContext *c, BEValue *ref)
{
	llvm_value_addr(c, ref);
	Type *type = ref->type;
	if (!type_is_abi_aggregate(type))
	{
		llvm_store_bevalue_raw(c, ref, llvm_get_zero(c, type));
		return;
	}
	Type *single_type = type_abi_find_single_struct_element(type);

	if (single_type && !type_is_abi_aggregate(single_type))
	{
		BEValue element;
		llvm_value_set_address_align(&element, llvm_emit_bitcast(c, ref->value, type_get_ptr(single_type)), single_type, (unsigned)ref->alignment);
		llvm_emit_memclear(c, &element);
		return;
	}
	if (type_size(type) <= 16)
	{
		if (type->type_kind == TYPE_STRUCT)
		{
			Decl *decl = type->decl;
			Decl **members = decl->strukt.members;
			VECEACH(members, i)
			{
				if (!type_size(members[i]->type)) continue;
				BEValue member_ref;
				llvm_emit_struct_member_ref(c, ref, &member_ref, i);
				llvm_emit_memclear(c, &member_ref);
			}
			return;
		}
		if (type->type_kind == TYPE_ARRAY)
		{
			LLVMTypeRef array_type = llvm_get_type(c, type);
			for (unsigned i = 0; i < type->array.len; i++)
			{
				AlignSize align;
				LLVMValueRef element_ptr = llvm_emit_array_gep_raw(c, ref->value, array_type, i, ref->alignment, &align);
				BEValue be_value;
				llvm_value_set_address_align(&be_value, element_ptr, type->array.base, align);
				llvm_emit_memclear(c, &be_value);
			}
			return;
		}
	}
	llvm_emit_memclear_size_align(c, ref->value, type_size(ref->type), ref->alignment, true);
}

LLVMValueRef llvm_emit_const_array_padding(LLVMTypeRef element_type, IndexDiff diff, bool *modified)
{
	if (diff == 1) return LLVMConstNull(element_type);
	*modified = true;
	LLVMTypeRef padding_type = LLVMArrayType(element_type, (unsigned)diff);
	return LLVMConstNull(padding_type);
}

LLVMValueRef llvm_emit_const_initializer(GenContext *c, ConstInitializer *const_init)
{
	switch (const_init->kind)
	{
		case CONST_INIT_ZERO:
			return LLVMConstNull(llvm_get_type(c, const_init->type));
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
			for (MemberIndex i = 0; i < size; i++)
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
				return LLVMConstStructInContext(c->context, parts, vec_size(parts), true);
			}
			return LLVMConstArray(element_type_llvm, parts, vec_size(parts));
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
				if (LLVMTypeOf(value) == element_type_llvm) was_modified = true;
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
				return LLVMConstStructInContext(c->context, parts, vec_size(parts), pack);
			}
			return LLVMConstArray(element_type_llvm, parts, vec_size(parts));
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
				return LLVMConstStructInContext(c->context, values, value_count, false);
			}

			return LLVMConstNamedStruct(union_type_llvm, values, value_count);
		}
		case CONST_INIT_STRUCT:
		{
			if (const_init->type->type_kind == TYPE_BITSTRUCT)
			{
				return llvm_emit_const_bitstruct(c, const_init);
			}
			Decl *decl = const_init->type->decl;
			Decl **members = decl->strukt.members;
			uint32_t count = vec_size(members);
			LLVMValueRef *entries = NULL;
			bool was_modified = false;
			for (MemberIndex i = 0; i < count; i++)
			{
				if (members[i]->padding)
				{
					vec_add(entries, llvm_emit_const_padding(c, members[i]->padding));
				}
				LLVMTypeRef expected_type = llvm_get_type(c, const_init->init_struct[i]->type);
				LLVMValueRef element = llvm_emit_const_initializer(c, const_init->init_struct[i]);
				LLVMTypeRef element_type = LLVMTypeOf(element);
				if (expected_type != element_type)
				{
					was_modified = true;
				}
				AlignSize new_align = llvm_abi_alignment(c, element_type);
				AlignSize expected_align = llvm_abi_alignment(c, expected_type);
				if (i != 0 && new_align < expected_align)
				{
					vec_add(entries, llvm_emit_const_padding(c, expected_align - new_align));
				}
				vec_add(entries, element);
			}
			if (decl->strukt.padding)
			{
				vec_add(entries, llvm_emit_const_padding(c, decl->strukt.padding));
			}
			if (was_modified)
			{
				LLVMValueRef value = LLVMConstStructInContext(c->context, entries, vec_size(entries), decl->is_packed);
				return value;
			}
			return LLVMConstNamedStruct(llvm_get_type(c, const_init->type), entries, vec_size(entries));
		}
		case CONST_INIT_VALUE:
		{
			BEValue value;
			llvm_emit_expr(c, &value, const_init->init_value);
			return llvm_value_rvalue_store(c, &value);
		}
	}
	UNREACHABLE
}


static void gencontext_emit_global_variable_definition(GenContext *c, Decl *decl)
{
	assert(decl->var.kind == VARDECL_GLOBAL || decl->var.kind == VARDECL_CONST);

	// Skip real constants.
	if (!decl->type) return;

	if (decl->type != type_void)
	{
		decl->backend_ref = LLVMAddGlobal(c->module, llvm_get_type(c, decl->type), "tempglobal");
	}
	if (IS_FAILABLE(decl))
	{
		scratch_buffer_clear();
		scratch_buffer_append(decl->external_name);
		scratch_buffer_append(".f");
		decl->var.failable_ref = LLVMAddGlobal(c->module, llvm_get_type(c, type_anyerr), scratch_buffer_to_string());
	}
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
			return;
		case TYPE_SUBARRAY:
		{
			// TODO insert trap on overflow.
			assert(value->kind == BE_ADDRESS);
			BEValue member;
			llvm_emit_subarray_pointer(c, value, &member);
			llvm_value_rvalue(c, &member);
			llvm_value_set_address_align(value, member.value, type_get_ptr(value->type->array.base), type_abi_alignment(value->type->array.base));
			return;
		}
		case TYPE_STRLIT:
			return;
		default:
			UNREACHABLE
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
	AlignSize alignment = type_alloca_alignment(var_type);

	Expr *init_expr = decl->var.init_expr;
	if (init_expr)
	{
		if (init_expr->expr_kind == EXPR_CONST && init_expr->const_expr.const_kind == CONST_LIST)
		{
			ConstInitializer *list = init_expr->const_expr.list;
			init_value = llvm_emit_const_initializer(c, list);
		}
		else
		{
			BEValue value;
			if (init_expr->expr_kind == EXPR_CONST && init_expr->const_expr.const_kind == CONST_BYTES)
			{
				init_value = LLVMConstStringInContext(c->context,
													  init_expr->const_expr.bytes.ptr,
													  init_expr->const_expr.bytes.len,
													  1);
			}
			else
			{
				llvm_emit_expr(c, &value, decl->var.init_expr);
				init_value = llvm_value_rvalue_store(c, &value);
			}
		}
	}
	else
	{
		init_value = LLVMConstNull(llvm_get_type(c, var_type));
	}


	// TODO fix name
	LLVMValueRef old = decl->backend_ref;
	decl->backend_ref = LLVMAddGlobal(c->module, LLVMTypeOf(init_value), decl->extname ? decl->extname : decl->external_name);
	LLVMSetThreadLocal(decl->backend_ref, decl->var.is_threadlocal);
	if (decl->section)
	{
		LLVMSetSection(decl->backend_ref, decl->section);
	}
	llvm_set_alignment(decl->backend_ref, alignment);

	LLVMValueRef failable_ref = decl->var.failable_ref;
	if (failable_ref)
	{
		llvm_set_alignment(failable_ref, type_alloca_alignment(type_anyerr));
		LLVMSetThreadLocal(failable_ref, decl->var.is_threadlocal);
	}
	if (init_expr && IS_FAILABLE(init_expr) && init_expr->expr_kind == EXPR_FAILABLE)
	{
		UNREACHABLE
	}
	if (decl->visibility != VISIBLE_EXTERN)
	{
		LLVMSetInitializer(decl->backend_ref, init_value);
		if (failable_ref)
		{
			LLVMSetInitializer(failable_ref, LLVMConstNull(llvm_get_type(c, type_anyerr)));
		}
	}

	LLVMSetGlobalConstant(decl->backend_ref, decl->var.kind == VARDECL_CONST);

	switch (decl->visibility)
	{
		case VISIBLE_MODULE:
			LLVMSetVisibility(decl->backend_ref, LLVMProtectedVisibility);
			if (failable_ref) LLVMSetVisibility(failable_ref, LLVMProtectedVisibility);
			break;
		case VISIBLE_PUBLIC:
			LLVMSetVisibility(decl->backend_ref, LLVMDefaultVisibility);
			if (failable_ref) LLVMSetVisibility(failable_ref, LLVMDefaultVisibility);
			break;
		case VISIBLE_EXTERN:
			LLVMSetLinkage(decl->backend_ref, LLVMExternalLinkage);
			if (failable_ref) LLVMSetLinkage(failable_ref, LLVMExternalLinkage);
			//LLVMSetVisibility(decl->backend_ref, LLVMDefaultVisibility);
			break;
		case VISIBLE_LOCAL:
			LLVMSetVisibility(decl->backend_ref, LLVMHiddenVisibility);
			if (failable_ref) LLVMSetVisibility(failable_ref, LLVMHiddenVisibility);
			break;
	}

	if (init_value && LLVMTypeOf(init_value) != llvm_get_type(c, var_type))
	{
		decl->backend_ref = LLVMConstBitCast(decl->backend_ref, llvm_get_ptr_type(c, var_type));
	}
	LLVMReplaceAllUsesWith(old, decl->backend_ref);
	LLVMDeleteGlobal(old);

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
			error_exit("Could not verify IR: %s", error);
		}
		error_exit("Could not verify module IR.");
	}
}

void gencontext_emit_object_file(GenContext *context)
{
	char *err = "";
	LLVMSetTarget(context->module, platform_target.target_triple);
	char *layout = LLVMCopyStringRepOfTargetData(context->target_data);
	LLVMSetDataLayout(context->module, layout);
	LLVMDisposeMessage(layout);

	// Generate .o or .obj file
	if (LLVMTargetMachineEmitToFile(context->machine, context->module, context->object_filename, LLVMObjectFile, &err))
	{
		error_exit("Could not emit object file: %s", err);
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
	assert(c->builder);
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
	decl->backend_ref = llvm_emit_alloca(c, llvm_get_type(c, type), decl->alignment, decl->name ? decl->name : "anon");
}

void llvm_emit_local_var_alloca(GenContext *c, Decl *decl)
{
	assert(!decl->var.is_static);
	llvm_emit_and_set_decl_alloca(c, decl);
	if (llvm_use_debug(c))
	{
		llvm_emit_debug_local_var(c, decl);
	}

}

/**
 * Values here taken from LLVM.
 * @return return the inlining threshold given the build options.
 */
static int get_inlining_threshold()
{
	if (active_target.optimization_level == OPTIMIZATION_AGGRESSIVE)
	{
		return 250;
	}
	switch (active_target.size_optimization_level)
	{
		case SIZE_OPTIMIZATION_TINY:
			return 5;
		case SIZE_OPTIMIZATION_SMALL:
			return 50;
		default:
			return 250;
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

void llvm_codegen_setup()
{
	if (intrinsics_setup) return;

	intrinsic_id.trap = lookup_intrinsic("llvm.trap");
	intrinsic_id.assume = lookup_intrinsic("llvm.assume");

	intrinsic_id.bswap = lookup_intrinsic("llvm.bswap");

	intrinsic_id.lifetime_start = lookup_intrinsic("llvm.lifetime.start");
	intrinsic_id.lifetime_end = lookup_intrinsic("llvm.lifetime.end");

	intrinsic_id.ssub_overflow = lookup_intrinsic("llvm.ssub.with.overflow");
	intrinsic_id.ssub_sat = lookup_intrinsic("llvm.ssub.sat");
	intrinsic_id.usub_overflow = lookup_intrinsic("llvm.usub.with.overflow");
	intrinsic_id.usub_sat = lookup_intrinsic("llvm.usub.sat");
	intrinsic_id.sadd_overflow = lookup_intrinsic("llvm.sadd.with.overflow");
	intrinsic_id.sadd_sat = lookup_intrinsic("llvm.sadd.sat");
	intrinsic_id.uadd_overflow = lookup_intrinsic("llvm.uadd.with.overflow");
	intrinsic_id.uadd_sat = lookup_intrinsic("llvm.uadd.sat");
	intrinsic_id.smul_overflow = lookup_intrinsic("llvm.smul.with.overflow");
	intrinsic_id.umul_overflow = lookup_intrinsic("llvm.umul.with.overflow");
	//intrinsic_id.sshl_sat = lookup_intrinsic("llvm.sshl.sat");
	//intrinsic_id.ushl_sat = lookup_intrinsic("llvm.ushl.sat");
	intrinsic_id.fshl = lookup_intrinsic("llvm.fshl");
	intrinsic_id.fshr = lookup_intrinsic("llvm.fshr");
	intrinsic_id.bitreverse = lookup_intrinsic("llvm.bitreverse");
	intrinsic_id.bswap = lookup_intrinsic("llvm.bswap");
	intrinsic_id.ctpop = lookup_intrinsic("llvm.ctpop");
	intrinsic_id.cttz = lookup_intrinsic("llvm.cttz");
	intrinsic_id.ctlz = lookup_intrinsic("llvm.ctlz");

	intrinsic_id.rint = lookup_intrinsic("llvm.rint");
	intrinsic_id.trunc = lookup_intrinsic("llvm.trunc");
	intrinsic_id.ceil = lookup_intrinsic("llvm.ceil");
	intrinsic_id.floor = lookup_intrinsic("llvm.floor");
	intrinsic_id.sqrt = lookup_intrinsic("llvm.sqrt");
	intrinsic_id.powi = lookup_intrinsic("llvm.powi");
	intrinsic_id.pow = lookup_intrinsic("llvm.pow");
	intrinsic_id.sin = lookup_intrinsic("llvm.sin");
	intrinsic_id.cos = lookup_intrinsic("llvm.cos");
	intrinsic_id.exp = lookup_intrinsic("llvm.exp");
	intrinsic_id.exp2 = lookup_intrinsic("llvm.exp2");
	intrinsic_id.log = lookup_intrinsic("llvm.log");
	intrinsic_id.log2 = lookup_intrinsic("llvm.log2");
	intrinsic_id.log10 = lookup_intrinsic("llvm.log10");
	intrinsic_id.fabs = lookup_intrinsic("llvm.fabs");
	intrinsic_id.fma = lookup_intrinsic("llvm.fma");
	intrinsic_id.copysign = lookup_intrinsic("llvm.copysign");
	intrinsic_id.minnum = lookup_intrinsic("llvm.minnum");
	intrinsic_id.maxnum = lookup_intrinsic("llvm.maxnum");
	intrinsic_id.minimum = lookup_intrinsic("llvm.minimum");
	intrinsic_id.maximum = lookup_intrinsic("llvm.maximum");
	intrinsic_id.convert_to_fp16 = lookup_intrinsic("llvm.convert.to.fp16");
	intrinsic_id.convert_from_fp16 = lookup_intrinsic("llvm.convert.from.fp16");
	intrinsic_id.nearbyint = lookup_intrinsic("llvm.nearbyint");
	intrinsic_id.roundeven = lookup_intrinsic("llvm.roundeven");
	intrinsic_id.lround = lookup_intrinsic("llvm.lround");
	intrinsic_id.llround = lookup_intrinsic("llvm.llround");
	intrinsic_id.lrint = lookup_intrinsic("llvm.lrint");
	intrinsic_id.llrint = lookup_intrinsic("llvm.llrint");

	//intrinsic_id.abs = lookup_intrinsic("llvm.abs");
	intrinsic_id.smax = lookup_intrinsic("llvm.smax");
	intrinsic_id.smin = lookup_intrinsic("llvm.smin");
	intrinsic_id.umax = lookup_intrinsic("llvm.umax");
	intrinsic_id.umin = lookup_intrinsic("llvm.umin");

	attribute_id.noinline = lookup_attribute("noinline");
	attribute_id.optnone = lookup_attribute("optnone");
	attribute_id.alwaysinline = lookup_attribute("alwaysinline");
	attribute_id.inlinehint = lookup_attribute("inlinehint");
	attribute_id.noreturn = lookup_attribute("noreturn");
	attribute_id.nounwind = lookup_attribute("nounwind");
	attribute_id.writeonly = lookup_attribute("writeonly");
	attribute_id.readonly = lookup_attribute("readonly");
	attribute_id.optnone = lookup_attribute("optnone");
	attribute_id.sret = lookup_attribute("sret");
	attribute_id.noalias = lookup_attribute("noalias");
	attribute_id.zext = lookup_attribute("zeroext");
	attribute_id.sext = lookup_attribute("signext");
	attribute_id.align = lookup_attribute("align");
	attribute_id.byval = lookup_attribute("byval");
	attribute_id.inreg = lookup_attribute("inreg");
	attribute_id.naked = lookup_attribute("naked");
	intrinsics_setup = true;
}

void llvm_set_linkage(GenContext *c, Decl *decl, LLVMValueRef value)
{
	if (decl->module != c->code_module)
	{
		LLVMSetLinkage(value, LLVMLinkOnceODRLinkage);
		LLVMSetVisibility(value, LLVMDefaultVisibility);
		return;
	}
	switch (decl->visibility)
	{
		case VISIBLE_MODULE:
		case VISIBLE_PUBLIC:
			LLVMSetLinkage(value, LLVMLinkOnceODRLinkage);
			LLVMSetVisibility(value, LLVMDefaultVisibility);
			break;
		case VISIBLE_EXTERN:
		case VISIBLE_LOCAL:
			LLVMSetVisibility(value, LLVMHiddenVisibility);
			LLVMSetLinkage(value, LLVMLinkerPrivateLinkage);
			break;
	}

}
void llvm_emit_introspection_type_from_decl(GenContext *c, Decl *decl)
{
	llvm_get_type(c, decl->type);
	if (decl_is_struct_type(decl))
	{
		Decl **decls = decl->strukt.members;
		VECEACH(decls, i)
		{
			Decl *member_decl = decls[i];
			if (decl_is_struct_type(member_decl))
			{
				llvm_emit_introspection_type_from_decl(c, member_decl);
			}
		}
	}
	if (decl_is_enum_kind(decl))
	{
		unsigned elements = vec_size(decl->enums.values);
		LLVMTypeRef element_type = llvm_get_type(c, type_voidptr);
		LLVMTypeRef elements_type = LLVMArrayType(element_type, elements);
		scratch_buffer_clear();
		scratch_buffer_append(decl->external_name);
		scratch_buffer_append("$elements");
		LLVMValueRef enum_elements = LLVMAddGlobal(c->module, elements_type, scratch_buffer_to_string());
		LLVMSetGlobalConstant(enum_elements, 1);
		llvm_set_linkage(c, decl, enum_elements);
		LLVMSetInitializer(enum_elements, LLVMConstNull(elements_type));
		AlignSize alignment = type_alloca_alignment(type_voidptr);
		for (unsigned i = 0; i < elements; i++)
		{
			AlignSize store_align;
			decl->enums.values[i]->backend_ref = llvm_emit_array_gep_raw(c, enum_elements, elements_type, i, alignment, &store_align);
		}
	}
	LLVMValueRef global_name = LLVMAddGlobal(c->module, llvm_get_type(c, type_char), decl->name ? decl->name : "anon");
	LLVMSetGlobalConstant(global_name, 1);
	LLVMSetInitializer(global_name, LLVMConstInt(llvm_get_type(c, type_char), 1, false));
	decl->type->backend_typeid = LLVMConstPointerCast(global_name, llvm_get_type(c, type_typeid));
	llvm_set_linkage(c, decl, global_name);
}


void llvm_value_set_bool(BEValue *value, LLVMValueRef llvm_value)
{
	value->value = llvm_value;
	value->alignment = type_abi_alignment(type_bool);
	value->kind = BE_BOOLEAN;
	value->type = type_bool;
}

void llvm_value_set_int(GenContext *c, BEValue *value, Type *type, uint64_t i)
{
	llvm_value_set(value, llvm_const_int(c, type, i), type);
}

void llvm_value_set(BEValue *value, LLVMValueRef llvm_value, Type *type)
{
	type = type_lowering(type);
	assert(llvm_value || type == type_void);
	value->value = llvm_value;
	value->alignment = type_abi_alignment(type);
	value->kind = BE_VALUE;
	value->type = type;
}

bool llvm_value_is_const(BEValue *value)
{
	return LLVMIsConstant(value->value);
}

void llvm_value_set_address_align(BEValue *value, LLVMValueRef llvm_value, Type *type, AlignSize alignment)
{
	value->value = llvm_value;
	value->alignment = alignment;
	value->kind = BE_ADDRESS;
	value->type = type_lowering(type);
}

void llvm_value_set_decl_address(BEValue *value, Decl *decl)
{
	decl = decl_flatten(decl);
	llvm_value_set_address(value, decl_ref(decl), decl->type);
	value->alignment = decl->alignment;

	if (decl->decl_kind == DECL_VAR && IS_FAILABLE(decl))
	{
		value->kind = BE_ADDRESS_FAILABLE;
		value->failable = decl->var.failable_ref;
	}
}

void llvm_value_set_address(BEValue *value, LLVMValueRef llvm_value, Type *type)
{
	llvm_value_set_address_align(value, llvm_value, type_lowering(type), type_abi_alignment(type));
}

void llvm_value_fold_failable(GenContext *c, BEValue *value)
{
	if (value->kind == BE_ADDRESS_FAILABLE)
	{
		LLVMBasicBlockRef after_block = llvm_basic_block_new(c, "after_check");
		BEValue error_value;
		llvm_value_set_address(&error_value, value->failable, type_anyerr);
		BEValue comp;
		llvm_value_set_bool(&comp, llvm_emit_is_no_error_value(c, &error_value));
		if (c->error_var)
		{
			LLVMBasicBlockRef error_block = llvm_basic_block_new(c, "error");
			llvm_emit_cond_br(c, &comp, after_block, error_block);
			llvm_emit_block(c, error_block);
			llvm_store_bevalue_dest_aligned(c, c->error_var, &error_value);
			llvm_emit_br(c, c->catch_block);
		}
		else
		{
			assert(c->catch_block);
			llvm_emit_cond_br(c, &comp, after_block, c->catch_block);
		}
		llvm_emit_block(c, after_block);
		value->kind = BE_ADDRESS;
	}
}

LLVMValueRef llvm_value_rvalue_store(GenContext *c, BEValue *value)
{
	llvm_value_fold_failable(c, value);
	switch (value->kind)
	{
		case BE_VALUE:
			return value->value;
		case BE_ADDRESS_FAILABLE:
			UNREACHABLE
		case BE_ADDRESS:
			return llvm_emit_load_aligned(c, llvm_get_type(c, value->type), value->value,
			                              value->alignment ? value->alignment : type_abi_alignment(value->type),
			                              "");
		case BE_BOOLEAN:
			if (LLVMIsConstant(value->value))
			{
				return LLVMConstZExt(value->value, c->byte_type);
			}
			return LLVMBuildZExt(c->builder, value->value, c->byte_type, "");
	}
	UNREACHABLE
}


LLVMBasicBlockRef llvm_basic_block_new(GenContext *c, const char *name)
{
	return LLVMCreateBasicBlockInContext(c->context, name);
}

void llvm_value_addr(GenContext *c, BEValue *value)
{
	llvm_value_fold_failable(c, value);
	if (value->kind == BE_ADDRESS) return;
	if (!c->builder)
	{
		LLVMValueRef val = llvm_value_rvalue_store(c, value);
		LLVMValueRef ref = LLVMAddGlobal(c->module, LLVMTypeOf(val), ".taddr");
		llvm_set_alignment(ref, llvm_abi_alignment(c, LLVMTypeOf(val)));
		llvm_set_private_linkage(ref);
		LLVMSetInitializer(ref, val);
		llvm_emit_bitcast(c, ref, type_get_ptr(value->type));
		llvm_value_set_address(value, ref, value->type);
	}
	else
	{
		LLVMValueRef temp = llvm_emit_alloca_aligned(c, value->type, "taddr");
		llvm_store_bevalue_dest_aligned(c, temp, value);
		llvm_value_set_address(value, temp, value->type);
	}
}

void llvm_value_rvalue(GenContext *c, BEValue *value)
{
	if (value->kind != BE_ADDRESS && value->kind != BE_ADDRESS_FAILABLE)
	{
		if (value->type->type_kind == TYPE_BOOL && value->kind != BE_BOOLEAN)
		{
			value->value = LLVMBuildTrunc(c->builder, value->value, c->bool_type, "");
			value->kind = BE_BOOLEAN;
		}
		return;
	}
	llvm_value_fold_failable(c, value);
	value->value = llvm_emit_load_aligned(c,
	                                      llvm_get_type(c, value->type),
	                                      value->value,
	                                      value->alignment ? value->alignment : type_abi_alignment(value->type),
	                                      "");
	if (value->type->type_kind == TYPE_BOOL)
	{
		value->value = LLVMBuildTrunc(c->builder, value->value, c->bool_type, "");
		value->kind = BE_BOOLEAN;
		return;
	}
	value->kind = BE_VALUE;
}


static void llvm_emit_type_decls(GenContext *context, Decl *decl)
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
		case DECL_DISTINCT:
			// TODO
			break;
		case DECL_ENUM_CONSTANT:
		case DECL_ERRVALUE:
			// TODO
			break;;
		case DECL_STRUCT:
		case DECL_UNION:
		case DECL_ENUM:
		case DECL_ERRTYPE:
		case DECL_BITSTRUCT:
			llvm_emit_introspection_type_from_decl(context, decl);
			break;
		case NON_TYPE_DECLS:
			UNREACHABLE
	}
}

const char *llvm_codegen(void *context)
{
	GenContext *c = context;
	LLVMModuleRef module = c->module;
	// Starting from here we could potentially thread this:
	LLVMPassManagerBuilderRef pass_manager_builder = LLVMPassManagerBuilderCreate();
	LLVMPassManagerBuilderSetOptLevel(pass_manager_builder, (unsigned)active_target.optimization_level);
	LLVMPassManagerBuilderSetSizeLevel(pass_manager_builder, (unsigned)active_target.size_optimization_level);
	LLVMPassManagerBuilderSetDisableUnrollLoops(pass_manager_builder, active_target.optimization_level == OPTIMIZATION_NONE);
	if (active_target.optimization_level != OPTIMIZATION_NONE)
	{
		LLVMPassManagerBuilderUseInlinerWithThreshold(pass_manager_builder, (unsigned)get_inlining_threshold());
	}
	LLVMPassManagerRef pass_manager = LLVMCreatePassManager();
	LLVMPassManagerRef function_pass_manager = LLVMCreateFunctionPassManagerForModule(module);
	LLVMAddAnalysisPasses(c->machine, function_pass_manager);
	LLVMAddAnalysisPasses(c->machine, pass_manager);
	LLVMPassManagerBuilderPopulateModulePassManager(pass_manager_builder, pass_manager);
	LLVMPassManagerBuilderPopulateFunctionPassManager(pass_manager_builder, function_pass_manager);

	// IMPROVE
	// In LLVM Opt, LoopVectorize and SLPVectorize settings are part of the PassManagerBuilder
	// Anything else we need to manually add?

	LLVMPassManagerBuilderDispose(pass_manager_builder);

	// Run function passes
	LLVMInitializeFunctionPassManager(function_pass_manager);
	LLVMValueRef current_function = LLVMGetFirstFunction(module);
	while (current_function)
	{
		LLVMRunFunctionPassManager(function_pass_manager, current_function);
		current_function = LLVMGetNextFunction(current_function);
	}
	LLVMFinalizeFunctionPassManager(function_pass_manager);
	LLVMDisposePassManager(function_pass_manager);

	// Run module pass
	LLVMRunPassManager(pass_manager, module);
	LLVMDisposePassManager(pass_manager);

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

	gencontext_end_module(c);
	gencontext_destroy(c);

	return object_name;
}

void *llvm_gen(Module *module)
{
	if (!vec_size(module->contexts)) return NULL;
	assert(intrinsics_setup);
	GenContext *gen_context = calloc(sizeof(GenContext), 1);
	gencontext_init(gen_context, module);
	gencontext_begin_module(gen_context);

	VECEACH(module->contexts, j)
	{
		Context *context = module->contexts[j];
		gencontext_init_file_emit(gen_context, context);
		gen_context->debug.compile_unit = context->llvm_debug_compile_unit;
		gen_context->debug.file = context->llvm_debug_file;

		VECEACH(context->external_symbol_list, i)
		{
			Decl *d = context->external_symbol_list[i];
			// Avoid duplicating symbol
			if (d->module == context->module) continue;
			llvm_emit_extern_decl(gen_context, context->external_symbol_list[i]);
		}
		VECEACH(context->methods, i)
		{
			llvm_emit_function_decl(gen_context, context->methods[i]);
		}
		VECEACH(context->types, i)
		{
			llvm_emit_type_decls(gen_context, context->types[i]);
		}
		VECEACH(context->functions, i)
		{
			llvm_emit_function_decl(gen_context, context->functions[i]);
		}
	}

	VECEACH(module->contexts, j)
	{
		Context *context = module->contexts[j];
		gen_context->debug.compile_unit = context->llvm_debug_compile_unit;
		gen_context->debug.file = context->llvm_debug_file;

		VECEACH(context->vars, i)
		{
			gencontext_emit_global_variable_definition(gen_context, context->vars[i]);
		}
		VECEACH(context->vars, i)
		{
			llvm_emit_global_variable_init(gen_context, context->vars[i]);
		}
		VECEACH(context->functions, i)
		{
			Decl *decl = context->functions[i];
			if (decl->func_decl.body) llvm_emit_function_body(gen_context, decl);
		}
		VECEACH(context->methods, i)
		{
			Decl *decl = context->methods[i];
			if (decl->func_decl.body) llvm_emit_function_body(gen_context, decl);
		}

		gencontext_end_file_emit(gen_context, context);
	}
	// EmitDeferred()

	if (llvm_use_debug(gen_context)) LLVMDIBuilderFinalize(gen_context->debug.builder);

	// If it's in test, then we want to serialize the IR before it is optimized.
	if (active_target.test_output)
	{
		gencontext_print_llvm_ir(gen_context);
		gencontext_verify_ir(gen_context);
	}
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

LLVMValueRef llvm_store_bevalue_aligned(GenContext *c, LLVMValueRef destination, BEValue *value, AlignSize alignment)
{
	// If we have an address but not an aggregate, do a load.
	llvm_value_fold_failable(c, value);
	if (value->kind == BE_ADDRESS && !type_is_abi_aggregate(value->type))
	{
		value->value = llvm_emit_load_aligned(c, llvm_get_type(c, value->type), value->value, value->alignment, "");
		value->kind = BE_VALUE;
	}
	switch (value->kind)
	{
		case BE_BOOLEAN:
			value->value = LLVMBuildZExt(c->builder, value->value, c->byte_type, "");
			value->kind = BE_VALUE;
			FALLTHROUGH;
		case BE_VALUE:
			return llvm_store_aligned(c, destination, value->value, alignment ? alignment : type_abi_alignment(value->type));
		case BE_ADDRESS_FAILABLE:
			UNREACHABLE
		case BE_ADDRESS:
		{
			// Here we do an optimized(?) memcopy.
			ByteSize size = type_size(value->type);
			LLVMValueRef copy_size = llvm_const_int(c, size <= UINT32_MAX ? type_uint : type_usize, size);
			destination = LLVMBuildBitCast(c->builder, destination, llvm_get_ptr_type(c, type_char), "");
			LLVMValueRef source = LLVMBuildBitCast(c->builder, value->value, llvm_get_ptr_type(c, type_char), "");
			LLVMValueRef copy = LLVMBuildMemCpy(c->builder,
			                                    destination,
			                                    alignment ? alignment : type_abi_alignment(value->type),
			                                    source,
			                                    value->alignment ? value->alignment : type_abi_alignment(value->type),
			                                    copy_size);
			return copy;
		}
	}
	UNREACHABLE
}

void llvm_store_bevalue_dest_aligned(GenContext *c, LLVMValueRef destination, BEValue *value)
{
	llvm_store_bevalue_aligned(c, destination, value, LLVMGetAlignment(destination));
}

LLVMValueRef llvm_store_bevalue(GenContext *c, BEValue *destination, BEValue *value)
{
	if (value->type == type_void) return NULL;
	assert(llvm_value_is_addr(destination));
	return llvm_store_bevalue_aligned(c, destination->value, value, destination->alignment);
}

void llvm_store_bevalue_raw(GenContext *c, BEValue *destination, LLVMValueRef raw_value)
{
	assert(llvm_value_is_addr(destination));
	llvm_store_aligned(c, destination->value, raw_value, destination->alignment);
}

void llvm_store_self_aligned(GenContext *context, LLVMValueRef pointer, LLVMValueRef value, Type *type)
{
	llvm_store_aligned(context, pointer, value, type_abi_alignment(type));
}

LLVMValueRef llvm_store_aligned(GenContext *context, LLVMValueRef pointer, LLVMValueRef value, AlignSize alignment)
{
	LLVMValueRef ref = LLVMBuildStore(context->builder, value, pointer);
	if (alignment) llvm_set_alignment(ref, alignment);
	return ref;
}

void llvm_store_aligned_decl(GenContext *context, Decl *decl, LLVMValueRef value)
{
	llvm_store_aligned(context, decl->backend_ref, value, decl->alignment);
}

void llvm_emit_memcpy(GenContext *c, LLVMValueRef dest, unsigned dest_align, LLVMValueRef source, unsigned src_align, uint64_t len)
{
	assert(dest_align && src_align);
	if (len <= UINT32_MAX)
	{
		LLVMBuildMemCpy(c->builder, dest, dest_align, source, src_align, llvm_const_int(c, type_uint, len));
		return;
	}
	LLVMBuildMemCpy(c->builder, dest, dest_align, source, src_align, llvm_const_int(c, type_ulong, len));
}

void llvm_emit_memcpy_to_decl(GenContext *c, Decl *decl, LLVMValueRef source, unsigned source_alignment)
{
	if (source_alignment == 0) source_alignment = type_abi_alignment(decl->type);
	llvm_emit_memcpy(c, decl->backend_ref, decl->alignment, source, source_alignment, type_size(decl->type));
}

LLVMValueRef llvm_emit_load_aligned(GenContext *c, LLVMTypeRef type, LLVMValueRef pointer, AlignSize alignment, const char *name)
{
	assert(c->builder);
	LLVMValueRef value = LLVMBuildLoad2(c->builder, type, pointer, name);
	assert(LLVMGetTypeContext(type) == c->context);
	llvm_set_alignment(value, alignment ? alignment : llvm_abi_alignment(c, type));
	return value;
}

TypeSize llvm_store_size(GenContext *c, LLVMTypeRef type)
{
	return (TypeSize)LLVMStoreSizeOfType(c->target_data, type);
}

void llvm_set_error_exit(GenContext *c, LLVMBasicBlockRef block)
{
	c->catch_block = block;
	c->error_var = NULL;
}

void llvm_set_error_exit_and_value(GenContext *c, LLVMBasicBlockRef block, LLVMValueRef ref)
{
	c->catch_block = block;
	c->error_var = ref;
}
