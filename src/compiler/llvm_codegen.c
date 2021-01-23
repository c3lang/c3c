// Copyright (c) 2019 Christoffer Lerno. All rights reserved.
// Use of this source code is governed by the GNU LGPLv3.0 license
// a copy of which can be found in the LICENSE file.

#include "llvm_codegen_internal.h"


static inline void llvm_set_alignment(LLVMValueRef alloca, LLVMTypeRef type, AlignSize alignment)
{
	LLVMSetAlignment(alloca, alignment ?: llvm_abi_alignment(type));
}

static int get_inlining_threshold(void);
static void diagnostics_handler(LLVMDiagnosticInfoRef ref, void *context)
{
	char *message = LLVMGetDiagInfoDescription(ref);
	LLVMDiagnosticSeverity severity = LLVMGetDiagInfoSeverity(ref);
	const char *severerity_name = "unknown";
	switch (severity)
	{
		case LLVMDSError:
			error_exit("LLVM error generating code for %s: %s", ((GenContext *)context)->ast_context->module->name, message);
			break;
		case LLVMDSWarning:
			severerity_name = "warning";
			break;
		case LLVMDSRemark:
			severerity_name = "remark";
			break;
		case LLVMDSNote:
			severerity_name = "note";
			break;
	}
	DEBUG_LOG("LLVM message [%s]: %s ", severerity_name, message);
	LLVMDisposeMessage(message);
}

static void gencontext_init(GenContext *context, Context *ast_context)
{
	memset(context, 0, sizeof(GenContext));
	context->context = LLVMContextCreate();
	context->bool_type = LLVMInt1TypeInContext(context->context);
	context->byte_type = LLVMInt8TypeInContext(context->context);
	LLVMContextSetDiagnosticHandler(context->context, &diagnostics_handler, context);
	context->ast_context = ast_context;
}

static void gencontext_destroy(GenContext *context)
{
	free(context->ir_filename);
	free(context->object_filename);
	LLVMContextDispose(context->context);
	free(context);
}

LLVMValueRef llvm_emit_memclear_size_align(GenContext *c, LLVMValueRef ref, uint64_t size, unsigned int align, bool bitcast)
{
	LLVMValueRef target = bitcast ? LLVMBuildBitCast(c->builder, ref, llvm_get_type(c, type_get_ptr(type_byte)), "") : ref;
	return LLVMBuildMemSet(c->builder, target, LLVMConstInt(llvm_get_type(c, type_byte), 0, false),
	                       LLVMConstInt(llvm_get_type(c, type_ulong), size, false), align);

}

LLVMValueRef llvm_emit_memclear(GenContext *c, BEValue *ref)
{
	// TODO avoid bitcast on those that do not need them.
	llvm_value_addr(c, ref);
	return llvm_emit_memclear_size_align(c, ref->value, type_size(ref->type), ref->alignment, true);
}

LLVMValueRef llvm_emit_const_array_padding(LLVMTypeRef element_type, IndexDiff diff, bool *modified)
{
	if (diff == 1) return LLVMConstNull(element_type);
	*modified = true;
	LLVMTypeRef padding_type = LLVMArrayType(element_type, (unsigned)diff);
	return LLVMConstNull(padding_type);
}

LLVMValueRef llvm_emit_const_initializer(GenContext *c, ConstInitializer *const_init, bool *modified)
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
			ArrayIndex size = array_type->array.len;
			assert(size > 0);
			LLVMValueRef *parts = VECNEW(LLVMValueRef, size);
			for (ArrayIndex i = 0; i < size; i++)
			{
				vec_add(parts, llvm_emit_const_initializer(c, elements[i], &was_modified));
			}
			if (was_modified) *modified = was_modified;
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
			ConstInitializer **elements = const_init->init_array.elements;
			unsigned element_count = vec_size(elements);
			assert(element_count > 0 && "Array should always have gotten at least one element.");
			ArrayIndex current_index = 0;
			unsigned alignment = 0;
			LLVMValueRef *parts = NULL;
			bool pack = false;
			VECEACH(elements, i)
			{
				ConstInitializer *element = elements[i];
				assert(element->kind == CONST_INIT_ARRAY_VALUE);
				ArrayIndex element_index = element->init_array_value.index;
				IndexDiff diff = element_index - current_index;
				unsigned curr_alignment = type_abi_alignment(element->type);
				if (alignment && curr_alignment != alignment)
				{
					pack = true;
				}
				alignment = curr_alignment;
				// Add zeroes
				if (diff > 0)
				{
					vec_add(parts, llvm_emit_const_array_padding(element_type_llvm, diff, &was_modified));
				}
				vec_add(parts, llvm_emit_const_initializer(c, element->init_array_value.element, &was_modified));
				current_index = element_index + 1;
			}

			IndexDiff end_diff = array_type->array.len - current_index;
			if (end_diff > 0)
			{
				vec_add(parts, llvm_emit_const_array_padding(element_type_llvm, end_diff, &was_modified));
			}
			if (was_modified) *modified = was_modified;
			if (was_modified)
			{
				return LLVMConstStructInContext(c->context, parts, vec_size(parts), pack);
			}
			return LLVMConstArray(element_type_llvm, parts, vec_size(parts));
		}
		case CONST_INIT_UNION:
		{
			Decl *decl = const_init->type->decl;
			// Get the type of the member that is used for this particular
			// constant value, for example if this is a union { double, int }
			// then the type may be double or int.
			Type *member_type = decl->strukt.members[const_init->init_union.index]->type->canonical;

			// Emit our value.
			LLVMValueRef result = llvm_emit_const_initializer(c, const_init->init_union.element, modified);

			// Get the union value
			LLVMTypeRef union_type_llvm = llvm_get_type(c, decl->type);

			// Take the first type in the union (note that there may be padding!)
			LLVMTypeRef first_type = LLVMStructGetTypeAtIndex(union_type_llvm, 0);

			// We need to calculate some possible padding.
			ByteSize union_size = type_size(const_init->type);
			ByteSize member_size = type_size(member_type);
			ByteSize padding = union_size - member_size;

			// Create the resulting values:
			LLVMValueRef values[2] = { result, NULL };
			unsigned value_count = 1;

			// Add possible padding as and i8 array.
			if (padding > 0)
			{
				values[1] = llvm_emit_const_padding(c, padding);
				value_count = 2;
			}

			// Is this another type than usual for the union?
			if (first_type != llvm_get_type(c, member_type))
			{
				// Yes, so the type needs to be modified.
				*modified = true;
			}

			// If it is modified we simply create a packed struct for representation.
			if (*modified)
			{
				return LLVMConstStructInContext(c->context, values, value_count, false);
			}

			return LLVMConstNamedStruct(union_type_llvm, values, value_count);
		}
		case CONST_INIT_STRUCT:
		{
			Decl *decl = const_init->type->decl;
			Decl **members = decl->strukt.members;
			MemberIndex count = vec_size(members);
			LLVMValueRef *entries = NULL;
			for (MemberIndex i = 0; i < count; i++)
			{
				if (members[i]->padding)
				{
					vec_add(entries, llvm_emit_const_padding(c, members[i]->padding));
				}
				vec_add(entries, llvm_emit_const_initializer(c, const_init->init_struct[i], modified));
			}
			if (decl->strukt.padding)
			{
				vec_add(entries, llvm_emit_const_padding(c, decl->strukt.padding));
			}
			if (*modified)
			{
				return LLVMConstStructInContext(c->context, entries, vec_size(entries), decl->is_packed);
			}
			return LLVMConstNamedStruct(llvm_get_type(c, const_init->type), entries, vec_size(entries));
		}
		case CONST_INIT_VALUE:
		{
			BEValue value;
			llvm_emit_expr(c, &value, const_init->init_value);
			LLVMValueRef llvm_value = llvm_value_rvalue_store(c, &value);
			LLVMTypeRef expected_type = llvm_get_type(c, const_init->type);
			if (expected_type != LLVMTypeOf(llvm_value)) *modified = true;
			return llvm_value;
		}
	}
	UNREACHABLE
}

static LLVMValueRef llvm_emit_const_initializer_simple(GenContext *c, Expr *expr, bool *modified)
{
	Expr **elements = expr->initializer_expr.initializer_expr;
	unsigned element_count = vec_size(elements);
	LLVMValueRef* values = malloc_arena(element_count * sizeof(LLVMValueRef));
	Type *expr_type = expr->type->canonical;
	LLVMTypeRef array_type = expr_type->type_kind == TYPE_ARRAY ? llvm_get_type(c, expr_type->array.base) : NULL;
	assert(array_type || type_is_structlike(expr_type));
	for (unsigned i = 0; i < element_count; i++)
	{
		BEValue value;
		llvm_emit_expr(c, &value, elements[i]);
		LLVMValueRef llvm_value = llvm_value_rvalue_store(c, &value);
		LLVMTypeRef expected_type = array_type ? array_type : llvm_get_type(c, expr_type->decl->strukt.members[0]->type);
		if (expected_type != LLVMTypeOf(llvm_value)) *modified = true;
		values[i] = llvm_value;
	}
	if (array_type)
	{
		if (*modified)
		{
			return LLVMConstStructInContext(c->context, values, element_count, true);
		}
		return LLVMConstArray(array_type, values, element_count);
	}
	if (*modified)
	{
		return LLVMConstStructInContext(c->context, values, element_count, true);
	}
	return LLVMConstNamedStruct(llvm_get_type(c, expr_type), values, element_count);
}

LLVMValueRef llvm_emit_const_aggregate(GenContext *c, Expr *expr, bool *modified)
{
	switch (expr->initializer_expr.init_type)
	{
		case INITIALIZER_UNKNOWN:
		case INITIALIZER_DESIGNATED:
			UNREACHABLE
		case INITIALIZER_CONST:
			return llvm_emit_const_initializer(c, expr->initializer_expr.initializer, modified);
		case INITIALIZER_NORMAL:
			return llvm_emit_const_initializer_simple(c, expr, modified);
	}
	UNREACHABLE
}

static void gencontext_emit_global_variable_definition(GenContext *c, Decl *decl)
{
	assert(decl->var.kind == VARDECL_GLOBAL || decl->var.kind == VARDECL_CONST);

	// Skip real constants.
	if (!decl->type) return;

	bool modified = false;
	LLVMValueRef init_value;

	ByteSize alignment = type_abi_alignment(decl->type);
	if (decl->var.init_expr)
	{
		if (decl->var.init_expr->expr_kind == EXPR_INITIALIZER_LIST)
		{
			init_value = llvm_emit_const_aggregate(c, decl->var.init_expr, &modified);
		}
		else
		{
			BEValue value;
			assert(decl->var.init_expr->expr_kind == EXPR_CONST);
			llvm_emit_expr(c, &value, decl->var.init_expr);
			init_value = llvm_value_rvalue_store(c, &value);
		}
	}
	else
	{
		init_value = LLVMConstNull(llvm_get_type(c, decl->type));
	}

	// TODO fix name
	decl->backend_ref = LLVMAddGlobal(c->module, LLVMTypeOf(init_value), decl->name);
	LLVMSetAlignment(decl->backend_ref, alignment);
	if (decl->visibility != VISIBLE_EXTERN)
	{
		LLVMSetInitializer(decl->backend_ref, init_value);
	}

	LLVMSetGlobalConstant(decl->backend_ref, decl->var.kind == VARDECL_CONST);

	switch (decl->visibility)
	{
		case VISIBLE_MODULE:
			LLVMSetVisibility(decl->backend_ref, LLVMProtectedVisibility);
			break;
		case VISIBLE_PUBLIC:
			LLVMSetVisibility(decl->backend_ref, LLVMDefaultVisibility);
			break;
		case VISIBLE_EXTERN:
			LLVMSetLinkage(decl->backend_ref, LLVMExternalLinkage);
			//LLVMSetVisibility(decl->backend_ref, LLVMDefaultVisibility);
			break;
		case VISIBLE_LOCAL:
			LLVMSetVisibility(decl->backend_ref, LLVMHiddenVisibility);
			break;
	}

	if (modified)
	{
		decl->backend_ref = LLVMConstBitCast(decl->backend_ref, llvm_get_ptr_type(c, decl->type));
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
			error_exit("Could not verify IR: %s", error);
		}
		error_exit("Could not verify module IR.");
	}
}

void gencontext_emit_object_file(GenContext *context)
{
	char *err = "";
	LLVMSetTarget(context->module, build_options.target_triple);
	char *layout = LLVMCopyStringRepOfTargetData(target_data_layout());
	LLVMSetDataLayout(context->module, layout);
	LLVMDisposeMessage(layout);

	// Generate .o or .obj file
	if (LLVMTargetMachineEmitToFile(target_machine(), context->module, context->object_filename, LLVMObjectFile, &err))
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


LLVMValueRef llvm_emit_alloca(GenContext *context, LLVMTypeRef type, unsigned alignment, const char *name)
{
	LLVMBasicBlockRef current_block = LLVMGetInsertBlock(context->builder);
	LLVMPositionBuilderBefore(context->builder, context->alloca_point);
	LLVMValueRef alloca = LLVMBuildAlloca(context->builder, type, name);
	llvm_set_alignment(alloca, type, alignment);
	LLVMPositionBuilderAtEnd(context->builder, current_block);
	return alloca;
}

LLVMValueRef llvm_emit_alloca_aligned(GenContext *c, Type *type, const char *name)
{
	return llvm_emit_alloca(c, llvm_get_type(c, type), type_alloca_alignment(type), name);
}

LLVMValueRef llvm_emit_decl_alloca(GenContext *c, Decl *decl)
{
	LLVMTypeRef type = llvm_get_type(c, decl->type);
	return llvm_emit_alloca(c,
	                        type,
	                        decl->alignment ?: type_alloca_alignment(decl->type),
	                        decl->name ?: "anon");
}

/**
 * Values here taken from LLVM.
 * @return return the inlining threshold given the build options.
 */
static int get_inlining_threshold(void)
{
	if (build_options.optimization_level == OPTIMIZATION_AGGRESSIVE)
	{
		return 250;
	}
	switch (build_options.size_optimization_level)
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
unsigned intrinsic_id_trap;
unsigned intrinsic_id_assume;

unsigned intrinsic_id_ssub_overflow;
unsigned intrinsic_id_ssub_sat;
unsigned intrinsic_id_usub_overflow;
unsigned intrinsic_id_usub_sat;
unsigned intrinsic_id_sadd_overflow;
unsigned intrinsic_id_sadd_sat;
unsigned intrinsic_id_uadd_overflow;
unsigned intrinsic_id_uadd_sat;
unsigned intrinsic_id_smul_overflow;
unsigned intrinsic_id_umul_overflow;
unsigned intrinsic_id_sshl_sat;
unsigned intrinsic_id_ushl_sat;
unsigned intrinsic_id_fmuladd;
unsigned intrinsic_id_rint;
unsigned intrinsic_id_trunc;
unsigned intrinsic_id_ceil;
unsigned intrinsic_id_floor;
unsigned intrinsic_id_sqrt;
unsigned intrinsic_id_nearbyint;
unsigned intrinsic_id_roundeven;
unsigned intrinsic_in_lround;
unsigned intrinsic_in_llround;
unsigned intrinsic_in_lrint;
unsigned intrinsic_in_llrint;
unsigned intrinsic_id_powi;
unsigned intrinsic_id_pow;
unsigned intrinsic_id_sin;
unsigned intrinsic_id_cos;
unsigned intrinsic_id_exp;
unsigned intrinsic_id_exp2;
unsigned intrinsic_id_log;
unsigned intrinsic_id_log10;
unsigned intrinsic_id_fabs;
unsigned intrinsic_id_fma;
unsigned intrinsic_id_copysign;
unsigned intrinsic_id_minnum;
unsigned intrinsic_id_maxnum;
unsigned intrinsic_id_minimum;
unsigned intrinsic_id_maximum;
unsigned intrinsic_id_smax;
unsigned intrinsic_id_smin;
unsigned intrinsic_id_umax;
unsigned intrinsic_id_umin;
unsigned intrinsic_id_abs;
unsigned intrinsic_id_fshl;
unsigned intrinsic_id_fshr;
unsigned intrinsic_id_bitreverse;
unsigned intrinsic_id_bswap;
unsigned intrinsic_id_ctpop;
unsigned intrinsic_id_ctlz;
unsigned intrinsic_id_cttz;
unsigned intrinsic_id_convert_from_fp16;
unsigned intrinsic_id_convert_to_fp16;




unsigned attribute_noinline;
unsigned attribute_alwaysinline;
unsigned attribute_inlinehint;
unsigned attribute_noreturn;
unsigned attribute_nounwind;
unsigned attribute_writeonly;
unsigned attribute_readonly;
unsigned attribute_optnone;
unsigned attribute_align;
unsigned attribute_noalias;
unsigned attribute_sret;
unsigned attribute_zext;
unsigned attribute_sext;
unsigned attribute_byval;
unsigned attribute_inreg;

void llvm_codegen_setup()
{
	assert(intrinsics_setup == false);
	intrinsic_id_trap = lookup_intrinsic("llvm.trap");
	intrinsic_id_assume = lookup_intrinsic("llvm.assume");






	intrinsic_id_ssub_overflow = lookup_intrinsic("llvm.ssub.with.overflow");
	intrinsic_id_ssub_sat = lookup_intrinsic("llvm.ssub.sat");
	intrinsic_id_usub_overflow = lookup_intrinsic("llvm.usub.with.overflow");
	intrinsic_id_usub_sat = lookup_intrinsic("llvm.usub.sat");
	intrinsic_id_sadd_overflow = lookup_intrinsic("llvm.sadd.with.overflow");
	intrinsic_id_sadd_sat = lookup_intrinsic("llvm.sadd.sat");
	intrinsic_id_uadd_overflow = lookup_intrinsic("llvm.uadd.with.overflow");
	intrinsic_id_uadd_sat = lookup_intrinsic("llvm.uadd.sat");
	intrinsic_id_smul_overflow = lookup_intrinsic("llvm.smul.with.overflow");
	intrinsic_id_umul_overflow = lookup_intrinsic("llvm.umul.with.overflow");
	//intrinsic_id_sshl_sat = lookup_intrinsic("llvm.sshl.sat");
	//intrinsic_id_ushl_sat = lookup_intrinsic("llvm.ushl.sat");
	intrinsic_id_fshl = lookup_intrinsic("llvm.fshl");
	intrinsic_id_fshr = lookup_intrinsic("llvm.fshr");
	intrinsic_id_bitreverse = lookup_intrinsic("llvm.bitreverse");
	intrinsic_id_bswap = lookup_intrinsic("llvm.bswap");
	intrinsic_id_ctpop = lookup_intrinsic("llvm.ctpop");
	intrinsic_id_cttz = lookup_intrinsic("llvm.cttz");
	intrinsic_id_ctlz = lookup_intrinsic("llvm.ctlz");

	intrinsic_id_rint = lookup_intrinsic("llvm.rint");
	intrinsic_id_trunc = lookup_intrinsic("llvm.trunc");
	intrinsic_id_ceil = lookup_intrinsic("llvm.ceil");
	intrinsic_id_floor = lookup_intrinsic("llvm.floor");
	intrinsic_id_sqrt = lookup_intrinsic("llvm.sqrt");
	intrinsic_id_powi = lookup_intrinsic("llvm.powi");
	intrinsic_id_pow = lookup_intrinsic("llvm.pow");
	intrinsic_id_sin = lookup_intrinsic("llvm.sin");
	intrinsic_id_cos = lookup_intrinsic("llvm.cos");
	intrinsic_id_exp = lookup_intrinsic("llvm.exp");
	intrinsic_id_exp2 = lookup_intrinsic("llvm.exp2");
	intrinsic_id_log = lookup_intrinsic("llvm.log");
	intrinsic_id_log10 = lookup_intrinsic("llvm.log10");
	intrinsic_id_fabs = lookup_intrinsic("llvm.fabs");
	intrinsic_id_fma = lookup_intrinsic("llvm.fma");
	intrinsic_id_copysign = lookup_intrinsic("llvm.copysign");
	intrinsic_id_minnum = lookup_intrinsic("llvm.minnum");
	intrinsic_id_maxnum = lookup_intrinsic("llvm.maxnum");
	intrinsic_id_minimum = lookup_intrinsic("llvm.minimum");
	intrinsic_id_maximum = lookup_intrinsic("llvm.maximum");
	intrinsic_id_convert_to_fp16 = lookup_intrinsic("llvm.convert.to.fp16");
	intrinsic_id_convert_from_fp16 = lookup_intrinsic("llvm.convert.from.fp16");
	intrinsic_id_nearbyint = lookup_intrinsic("llvm.nearbyint");
	intrinsic_id_roundeven = lookup_intrinsic("llvm.roundeven");
	intrinsic_in_lround = lookup_intrinsic("llvm.lround");
	intrinsic_in_llround = lookup_intrinsic("llvm.llround");
	intrinsic_in_lrint = lookup_intrinsic("llvm.lrint");
	intrinsic_in_llrint = lookup_intrinsic("llvm.llrint");

	//intrinsic_id_abs = lookup_intrinsic("llvm.abs");
	intrinsic_id_smax = lookup_intrinsic("llvm.smax");
	intrinsic_id_smin = lookup_intrinsic("llvm.smin");
	intrinsic_id_umax = lookup_intrinsic("llvm.umax");
	intrinsic_id_umin = lookup_intrinsic("llvm.umin");

	attribute_noinline = lookup_attribute("noinline");
	attribute_alwaysinline = lookup_attribute("alwaysinline");
	attribute_inlinehint = lookup_attribute("inlinehint");
	attribute_noreturn = lookup_attribute("noreturn");
	attribute_nounwind = lookup_attribute("nounwind");
	attribute_writeonly = lookup_attribute("writeonly");
	attribute_readonly = lookup_attribute("readonly");
	attribute_optnone = lookup_attribute("optnone");
	attribute_sret = lookup_attribute("sret");
	attribute_noalias = lookup_attribute("noalias");
	attribute_zext = lookup_attribute("zeroext");
	attribute_sext = lookup_attribute("signext");
	attribute_align = lookup_attribute("align");
	attribute_byval = lookup_attribute("byval");
	attribute_inreg = lookup_attribute("inreg");
	intrinsics_setup = true;
}

void gencontext_emit_introspection_type(GenContext *context, Decl *decl)
{
	llvm_get_type(context, decl->type);
	if (decl_is_struct_type(decl))
	{
		Decl **decls = decl->strukt.members;
		VECEACH(decls, i)
		{
			Decl *member_decl = decls[i];
			if (decl_is_struct_type(member_decl))
			{
				gencontext_emit_introspection_type(context, member_decl);
			}
		}
	}
	LLVMValueRef global_name = LLVMAddGlobal(context->module, llvm_get_type(context, type_byte), decl->name ? decl->name : "anon");
	LLVMSetGlobalConstant(global_name, 1);
	LLVMSetInitializer(global_name, LLVMConstInt(llvm_get_type(context, type_byte), 1, false));
	decl->type->backend_typeid = LLVMConstPointerCast(global_name, llvm_get_type(context, type_typeid));

	switch (decl->visibility)
	{
		case VISIBLE_MODULE:
		case VISIBLE_PUBLIC:
			LLVMSetLinkage(global_name, LLVMLinkOnceODRLinkage);
			LLVMSetVisibility(global_name, LLVMDefaultVisibility);
			break;
		case VISIBLE_EXTERN:
		case VISIBLE_LOCAL:
			LLVMSetVisibility(global_name, LLVMHiddenVisibility);
			LLVMSetLinkage(global_name, LLVMLinkerPrivateLinkage);
			break;
	}
}

static inline uint32_t upper_power_of_two(uint32_t v)
{
	v--;
	v |= v >> 1;
	v |= v >> 2;
	v |= v >> 4;
	v |= v >> 8;
	v |= v >> 16;
	v++;
	return v;
}

void llvm_value_set_bool(BEValue *value, LLVMValueRef llvm_value)
{
	value->value = llvm_value;
	value->alignment = type_abi_alignment(type_bool);
	value->kind = BE_BOOLEAN;
	value->type = type_bool;
}

void llvm_value_set(BEValue *value, LLVMValueRef llvm_value, Type *type)
{
	value->value = llvm_value;
	value->alignment = type_abi_alignment(type);
	value->kind = BE_VALUE;
	value->type = type;
}

bool llvm_value_is_const(BEValue *value)
{
	return LLVMIsConstant(value->value);
}

void llvm_value_set_address_align(BEValue *value, LLVMValueRef llvm_value, Type *type, unsigned alignment)
{
	value->value = llvm_value;
	value->alignment = alignment;
	value->kind = BE_ADDRESS;
	value->type = type;
}
void llvm_value_set_address(BEValue *value, LLVMValueRef llvm_value, Type *type)
{
	llvm_value_set_address_align(value, llvm_value, type, type_abi_alignment(type));
}

void llvm_value_fold_failable(GenContext *c, BEValue *value)
{
	if (value->kind == BE_ADDRESS_FAILABLE)
	{
		LLVMBasicBlockRef after_block = llvm_basic_block_new(c, "after_check");
		// TODO optimize load.
		LLVMValueRef error_value = gencontext_emit_load(c, type_error, value->failable);
		BEValue comp;
		llvm_value_set_bool(&comp, llvm_emit_is_no_error(c, error_value));
		if (c->error_var)
		{
			LLVMBasicBlockRef error_block = llvm_basic_block_new(c, "error");
			llvm_emit_cond_br(c, &comp, after_block, error_block);
			llvm_emit_block(c, error_block);
			llvm_store_aligned(c, c->error_var, error_value, type_abi_alignment(type_usize));
			llvm_emit_br(c, c->catch_block);
		}
		else
		{
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
			return llvm_emit_load_aligned(c,
			                              llvm_get_type(c, value->type),
			                              value->value,
			                              value->alignment ?: type_abi_alignment(value->type),
			                              "");
		case BE_BOOLEAN:
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
	LLVMValueRef temp = llvm_emit_alloca_aligned(c, value->type, "tempaddr");
	llvm_store_self_aligned(c, temp, value->value, value->type);
	llvm_value_set_address(value, temp, value->type);
}

void llvm_value_rvalue(GenContext *c, BEValue *value)
{
	if (value->kind != BE_ADDRESS && value->kind != BE_ADDRESS_FAILABLE) return;
	llvm_value_fold_failable(c, value);
	value->value = llvm_emit_load_aligned(c,
	                                      llvm_get_type(c, value->type),
	                                      value->value,
	                                      value->alignment ?: type_abi_alignment(value->type),
	                                      "");
	if (value->type->type_kind == TYPE_BOOL)
	{
		value->value = LLVMBuildTrunc(c->builder, value->value, c->bool_type, "");
		value->kind = BE_BOOLEAN;
		return;
	}
	value->kind = BE_VALUE;
}


static void gencontext_emit_decl(GenContext *context, Decl *decl)
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
			// TODO
			break;;
		case DECL_STRUCT:
		case DECL_UNION:
		case DECL_ERR:
			gencontext_emit_introspection_type(context, decl);
			break;
		case DECL_ENUM:
			// TODO
			break;
		case NON_TYPE_DECLS:
			UNREACHABLE
	}
}

void llvm_codegen(void *context)
{
	GenContext *gen_context = context;
	LLVMModuleRef module = gen_context->module;
	// Starting from here we could potentially thread this:
	LLVMPassManagerBuilderRef pass_manager_builder = LLVMPassManagerBuilderCreate();
	LLVMPassManagerBuilderSetOptLevel(pass_manager_builder, build_options.optimization_level);
	LLVMPassManagerBuilderSetSizeLevel(pass_manager_builder, build_options.size_optimization_level);
	LLVMPassManagerBuilderSetDisableUnrollLoops(pass_manager_builder, build_options.optimization_level == OPTIMIZATION_NONE);
	LLVMPassManagerBuilderUseInlinerWithThreshold(pass_manager_builder, get_inlining_threshold());
	LLVMPassManagerRef pass_manager = LLVMCreatePassManager();
	LLVMPassManagerRef function_pass_manager = LLVMCreateFunctionPassManagerForModule(module);
	LLVMAddAnalysisPasses(target_machine(), function_pass_manager);
	LLVMAddAnalysisPasses(target_machine(), pass_manager);
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
	if (build_options.emit_llvm)
	{
		gencontext_print_llvm_ir(gen_context);
		gencontext_verify_ir(gen_context);
	}

	if (build_options.emit_bitcode) gencontext_emit_object_file(gen_context);

	gencontext_end_module(gen_context);
	gencontext_destroy(gen_context);


}
void *llvm_gen(Context *context)
{
	assert(intrinsics_setup);
	GenContext *gen_context = calloc(sizeof(GenContext), 1);
	gencontext_init(gen_context, context);
	gencontext_begin_module(gen_context);
	// EmitDeferred()
	VECEACH(context->external_symbol_list, i)
	{
		llvm_emit_extern_decl(gen_context, context->external_symbol_list[i]);
	}
	VECEACH(context->methods, i)
	{
		llvm_emit_function_decl(gen_context, context->methods[i]);
	}
	VECEACH(context->functions, i)
	{
		llvm_emit_function_decl(gen_context, context->functions[i]);
	}
	VECEACH(context->types, i)
	{
		gencontext_emit_decl(gen_context, context->types[i]);
	}
	VECEACH(context->vars, i)
	{
		gencontext_emit_global_variable_definition(gen_context, context->vars[i]);
	}
	VECEACH(context->functions, i)
	{
		Decl *decl = context->functions[i];
		if (decl->func.body) llvm_emit_function_body(gen_context, decl);
	}
	VECEACH(context->methods, i)
	{
		Decl *decl = context->methods[i];
		if (decl->func.body) llvm_emit_function_body(gen_context, decl);
	}

	if (llvm_use_debug(gen_context)) LLVMDIBuilderFinalize(gen_context->debug.builder);

	// If it's in test, then we want to serialize the IR before it is optimized.
	if (build_options.test_mode)
	{
		gencontext_print_llvm_ir(gen_context);
		gencontext_verify_ir(gen_context);
	}
	return gen_context;
}

void llvm_attribute_add_int(GenContext *context, LLVMValueRef value_to_add_attribute_to, unsigned attribute_id, uint64_t val, int index)
{
	LLVMAttributeRef llvm_attr = LLVMCreateEnumAttribute(context->context, attribute_id, val);
	LLVMAddAttributeAtIndex(value_to_add_attribute_to, index, llvm_attr);
}

void llvm_attribute_add(GenContext *context, LLVMValueRef value_to_add_attribute_to, unsigned attribute_id, int index)
{
	llvm_attribute_add_int(context, value_to_add_attribute_to, attribute_id, 0, index);
}

void llvm_attribute_add_range(GenContext *context, LLVMValueRef value_to_add_attribute_to, unsigned attribute_id, int index_start, int index_end)
{
	for (int i = index_start; i <= index_end; i++)
	{
		llvm_attribute_add_int(context, value_to_add_attribute_to, attribute_id, 0, i);
	}
}

void llvm_attribute_add_string(GenContext *context, LLVMValueRef value_to_add_attribute_to, const char *attribute, const char *value, int index)
{
	LLVMAttributeRef llvm_attr = LLVMCreateStringAttribute(context->context, attribute, strlen(attribute), value, strlen(value));
	LLVMAddAttributeAtIndex(value_to_add_attribute_to, index, llvm_attr);
}

unsigned llvm_abi_size(LLVMTypeRef type)
{
	return LLVMABISizeOfType(target_data_layout(), type);
}

AlignSize llvm_abi_alignment(LLVMTypeRef type)
{
	return (AlignSize)LLVMABIAlignmentOfType(target_data_layout(), type);
}

void llvm_store_bevalue_aligned(GenContext *c, LLVMValueRef destination, BEValue *value, AlignSize alignment)
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
			FALLTHROUGH;
		case BE_VALUE:
			llvm_store_aligned(c, destination, value->value, alignment ?: type_abi_alignment(value->type));
			return;
		case BE_ADDRESS_FAILABLE:
			UNREACHABLE
		case BE_ADDRESS:
		{
			// Here we do an optimized(?) memcopy.
			ByteSize size = type_size(value->type);
			LLVMValueRef copy_size = llvm_const_int(c, size <= UINT32_MAX ? type_uint : type_usize, size);
			destination = LLVMBuildBitCast(c->builder, destination, llvm_get_ptr_type(c, type_byte), "");
			LLVMValueRef source = LLVMBuildBitCast(c->builder, value->value, llvm_get_ptr_type(c, type_byte), "");
			LLVMBuildMemCpy(c->builder, destination, alignment ?: type_abi_alignment(value->type),
			                source, value->alignment ?: type_abi_alignment(value->type), copy_size);
			return;
		}
	}
	UNREACHABLE
}

void llvm_store_bevalue_dest_aligned(GenContext *c, LLVMValueRef destination, BEValue *value)
{
	llvm_store_bevalue_aligned(c, destination, value, LLVMGetAlignment(destination));
}

void llvm_store_bevalue(GenContext *c, BEValue *destination, BEValue *value)
{
	assert(llvm_value_is_addr(destination));
	llvm_store_bevalue_aligned(c, destination->value, value, destination->alignment);
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

void llvm_store_aligned(GenContext *context, LLVMValueRef pointer, LLVMValueRef value, AlignSize alignment)
{
	LLVMValueRef ref = LLVMBuildStore(context->builder, value, pointer);
	if (alignment) LLVMSetAlignment(ref, alignment);
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
	llvm_emit_memcpy(c, decl->backend_ref, decl->alignment ?: type_abi_alignment(decl->type),
	                 source, source_alignment, type_size(decl->type));
}

LLVMValueRef llvm_emit_load_aligned(GenContext *context, LLVMTypeRef type, LLVMValueRef pointer, unsigned alignment, const char *name)
{
	LLVMValueRef value = LLVMBuildLoad2(context->builder, type, pointer, name);
	llvm_set_alignment(value, type, alignment);
	return value;
}

unsigned llvm_store_size(LLVMTypeRef type)
{
	return LLVMStoreSizeOfType(target_data_layout(), type);
}
