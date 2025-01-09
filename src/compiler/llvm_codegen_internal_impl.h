INLINE LLVMValueRef llvm_emit_insert_value(GenContext *c, LLVMValueRef agg, LLVMValueRef new_value, ArraySize index)
{
	if (LLVMGetTypeKind(LLVMTypeOf(agg)) == LLVMVectorTypeKind)
	{
		LLVMValueRef index_val = llvm_const_int(c, type_usz, index);
		return LLVMBuildInsertElement(c->builder, agg, new_value, index_val, "");
	}
	return LLVMBuildInsertValue(c->builder, agg, new_value, index, "");
}

INLINE bool llvm_is_int_or_vector_int(LLVMTypeRef type)
{
	if (LLVMGetTypeKind(type) == LLVMIntegerTypeKind) return true;
	return LLVMGetTypeKind(type) == LLVMVectorTypeKind
		&& LLVMGetTypeKind(LLVMGetElementType(type)) == LLVMIntegerTypeKind;
}

INLINE LLVMValueRef llvm_zext_trunc(GenContext *c, LLVMValueRef data, LLVMTypeRef type)
{
	LLVMTypeRef current_type = LLVMTypeOf(data);
	if (current_type == type) return data;
	ASSERT0(llvm_is_int_or_vector_int(type));
	ASSERT0(llvm_is_int_or_vector_int(current_type));
	if (llvm_bitsize(c, current_type) < llvm_bitsize(c, type))
	{
		return LLVMBuildZExt(c->builder, data, type, "zext");
	}
	ASSERT0(llvm_bitsize(c, current_type) > llvm_bitsize(c, type));
	return LLVMBuildTrunc(c->builder, data, type, "trunc");
}

INLINE LLVMValueRef llvm_sext_trunc(GenContext *c, LLVMValueRef data, LLVMTypeRef type)
{
	LLVMTypeRef current_type = LLVMTypeOf(data);
	if (current_type == type) return data;
	ASSERT0(llvm_is_int_or_vector_int(type));
	ASSERT0(llvm_is_int_or_vector_int(current_type));
	if (llvm_bitsize(c, current_type) < llvm_bitsize(c, type))
	{
		return LLVMBuildSExt(c->builder, data, type, "sext");
	}
	ASSERT0(llvm_bitsize(c, current_type) > llvm_bitsize(c, type));
	return LLVMBuildTrunc(c->builder, data, type, "trunc");
}

INLINE bool type_is_intlike(Type *type)
{
	type = type_flatten(type);
	if (type_is_integer_or_bool_kind(type)) return true;
	if (type->type_kind != TYPE_VECTOR) return false;
	type = type->array.base;
	return type_is_integer_or_bool_kind(type);
}

INLINE void llvm_value_ext_trunc(GenContext *c, BEValue *value, Type *type)
{
	type = type_flatten(type);
	Type *from_type = value->type;
	ByteSize size = type_size(from_type);
	ByteSize to_size = type_size(type);

	ASSERT0(type_is_intlike(type) && type_is_intlike(from_type));
	if (size == to_size) return;

	llvm_value_rvalue(c, value);
	LLVMTypeRef current_type = llvm_get_type(c, type);
	if (size < to_size)
	{
		if (type_is_signed(from_type))
		{
			value->value = LLVMBuildSExt(c->builder, value->value, current_type, "sext");
			value->type = type;
			return;
		}
		value->value = LLVMBuildZExt(c->builder, value->value, current_type, "zext");
		value->type = type;
		return;
	}
	value->value = LLVMBuildTrunc(c->builder, value->value, current_type, "trunc");
	value->type = type;
}

INLINE LLVMValueRef llvm_store_decl(GenContext *c, Decl *decl, BEValue *value)
{
	BEValue ref;
	llvm_value_set_decl(c, &ref, decl);
	ASSERT0(llvm_value_is_addr(&ref));
	return llvm_store(c, &ref, value);
}

INLINE LLVMValueRef llvm_store_raw(GenContext *c, BEValue *destination, LLVMValueRef raw_value)
{
	ASSERT0(llvm_value_is_addr(destination));
	return llvm_store_to_ptr_raw_aligned(c, destination->value, raw_value, destination->alignment);
}


INLINE LLVMValueRef llvm_store_decl_raw(GenContext *context, Decl *decl, LLVMValueRef value)
{
	ASSERT0(!decl->is_value);
	return llvm_store_to_ptr_raw_aligned(context, decl->backend_ref, value, decl->alignment);
}

INLINE AlignSize llvm_type_or_alloca_align(LLVMValueRef dest, Type *type)
{
	if (LLVMIsAAllocaInst(dest) || LLVMIsAGlobalVariable(dest))
	{
		return LLVMGetAlignment(dest);
	}
	return type_abi_alignment(type);
}

INLINE LLVMValueRef llvm_store_to_ptr(GenContext *c, LLVMValueRef destination, BEValue *value)
{
	return llvm_store_to_ptr_aligned(c, destination, value, llvm_type_or_alloca_align(destination, value->type));
}

INLINE LLVMValueRef llvm_store_to_ptr_raw(GenContext *c, LLVMValueRef pointer, LLVMValueRef value, Type *type)
{
	return llvm_store_to_ptr_raw_aligned(c, pointer, value, llvm_type_or_alloca_align(pointer, type));
}

INLINE void llvm_value_bitcast(GenContext *c UNUSED, BEValue *value, Type *type)
{
	ASSERT0(llvm_value_is_addr(value));
	type = type_lowering(type);
	value->type = type;
}

INLINE LLVMValueRef llvm_emit_shl(GenContext *c, LLVMValueRef value, LLVMValueRef shift)
{
	return LLVMBuildShl(c->builder, value, shift, "shl");
}

INLINE LLVMValueRef llvm_emit_ashr(GenContext *c, LLVMValueRef value, LLVMValueRef shift)
{
	return LLVMBuildAShr(c->builder, value, shift, "ashr");
}

INLINE LLVMValueRef llvm_emit_lshr(GenContext *c, LLVMValueRef value, LLVMValueRef shift)
{
	return LLVMBuildLShr(c->builder, value, shift, "lshrl");
}

INLINE LLVMValueRef llvm_emit_trunc_bool(GenContext *c, LLVMValueRef value)
{
	LLVMTypeRef type = LLVMTypeOf(value);
	if (LLVMGetTypeKind(type) == LLVMVectorTypeKind)
	{
		return LLVMBuildTrunc(c->builder, value, LLVMVectorType(c->bool_type, LLVMGetVectorSize(type)), "");
	}
	return LLVMBuildTrunc(c->builder, value, c->bool_type, "");
}

INLINE LLVMValueRef llvm_emit_extract_value(GenContext *c, LLVMValueRef agg, unsigned index)
{
	if (LLVMGetTypeKind(LLVMTypeOf(agg)) == LLVMVectorTypeKind)
	{
		return LLVMBuildExtractElement(c->builder, agg, llvm_const_int(c, type_usz, index), "");
	}
	return LLVMBuildExtractValue(c->builder, agg, index, "");
}

INLINE bool llvm_use_accurate_debug_info(GenContext *context)
{
	return context->debug.builder && compiler.build.optlevel <= OPTIMIZATION_NONE;
}

INLINE bool llvm_use_debug(GenContext *context) { return context->debug.builder != NULL; }

INLINE bool llvm_basic_block_is_unused(LLVMBasicBlockRef block)
{
	return !LLVMGetFirstInstruction(block) && !LLVMGetFirstUse(LLVMBasicBlockAsValue(block));
}

static inline bool llvm_delete_current_if_unused(GenContext *c)
{
	LLVMBasicBlockRef current = c->current_block;
	if (!current || !llvm_basic_block_is_unused(current)) return false;
	LLVMBasicBlockRef prev_block = LLVMGetPreviousBasicBlock(current);
	LLVMDeleteBasicBlock(current);
	c->current_block = prev_block;
	LLVMPositionBuilderAtEnd(c->builder, prev_block);
	return true;
}

static inline LLVMBasicBlockRef llvm_get_current_block_if_in_use(GenContext *context)
{
	LLVMBasicBlockRef block = context->current_block;
	if (block && llvm_basic_block_is_unused(block))
	{
		LLVMDeleteBasicBlock(block);
		return context->current_block = NULL;
	}
	return block;
}

static inline LLVMCallConv llvm_call_convention_from_call(CallABI abi)
{
	switch (abi)
	{
		case CALL_C:
			return LLVMCCallConv;
		case CALL_X64_VECTOR:
			return LLVMX86VectorCallCallConv;
		case CALL_AAPCS:
			return LLVMARMAAPCSCallConv;
		case CALL_AAPCS_VFP:
			return LLVMARMAAPCSVFPCallConv;
		default:
			return LLVMCCallConv;
	}

}

INLINE bool llvm_is_global_eval(GenContext *c)
{
	return c->builder == c->global_builder;
}

INLINE bool llvm_is_local_eval(GenContext *c)
{
	return c->builder != c->global_builder;
}

INLINE LLVMValueRef llvm_emit_and_raw(GenContext *c, LLVMValueRef lhs, LLVMValueRef rhs)
{
	if (llvm_is_const_null(lhs)) return lhs;
	if (llvm_is_const_null(rhs)) return rhs;
	return LLVMBuildAnd(c->builder, lhs, rhs, "");
}

INLINE LLVMValueRef llvm_emit_or_raw(GenContext *c, LLVMValueRef lhs, LLVMValueRef rhs)
{
	if (llvm_is_const_null(lhs)) return rhs;
	if (llvm_is_const_null(rhs)) return lhs;
	return LLVMBuildOr(c->builder, lhs, rhs, "");
}

INLINE LLVMValueRef llvm_emit_and(GenContext *c, BEValue *lhs, BEValue *rhs)
{
	return llvm_emit_and_raw(c, lhs->value, rhs->value);
}

INLINE LLVMValueRef llvm_get_zero(GenContext *c, Type *type)
{
	return LLVMConstNull(llvm_get_type(c, type));
}

INLINE LLVMValueRef llvm_get_zero_raw(LLVMTypeRef type)
{
	return LLVMConstNull(type);
}

INLINE LLVMValueRef llvm_get_undef(GenContext *c, Type *type)
{
	return LLVMGetUndef(llvm_get_type(c, type));
}

INLINE LLVMValueRef llvm_get_undef_raw(LLVMTypeRef type)
{
	return LLVMGetUndef(type);
}

INLINE LLVMValueRef llvm_get_ones_raw(LLVMTypeRef type)
{
	return LLVMConstAllOnes(type);
}

INLINE bool llvm_is_const_null(LLVMValueRef value)
{
	return LLVMIsNull(value);
}

INLINE bool llvm_is_const(LLVMValueRef value)
{
	return LLVMIsConstant(value);
}

INLINE LLVMValueRef llvm_get_zstring(GenContext *c, const char *str, size_t len)
{
	ASSERT0(len == (unsigned)len);
	return LLVMConstStringInContext(c->context, str, (unsigned)len, 0);
}

INLINE LLVMValueRef llvm_get_bytes(GenContext *c, const char *str, size_t len)
{
	ASSERT0(len == (unsigned)len);
	return LLVMConstStringInContext(c->context, str, (unsigned)len, 1);
}

INLINE LLVMValueRef llvm_get_struct(GenContext *c, LLVMValueRef *vals, size_t len)
{
	ASSERT0(len == (unsigned)len);
	return LLVMConstStructInContext(c->context, vals, (unsigned)len, false);
}

INLINE LLVMValueRef llvm_get_packed_struct(GenContext *c, LLVMValueRef *vals, size_t len)
{
	ASSERT0(len == (unsigned)len);
	return LLVMConstStructInContext(c->context, vals, (unsigned)len, true);
}

INLINE LLVMValueRef llvm_get_unnamed_struct(GenContext *c, LLVMValueRef *vals, bool is_packed)
{
	return LLVMConstStructInContext(c->context, vals, vec_size(vals), is_packed);
}

INLINE LLVMValueRef llvm_get_array(LLVMTypeRef type, LLVMValueRef *vals, unsigned count)
{
	return LLVMConstArray(type, vals, count);
}

INLINE LLVMValueRef llvm_get_struct_named(LLVMTypeRef type, LLVMValueRef *vals, unsigned count)
{
	return LLVMConstNamedStruct(type, vals, count);
}

INLINE LLVMValueRef llvm_get_struct_of_type(GenContext *c, Type *type, LLVMValueRef *vals, unsigned count)
{
	return LLVMConstNamedStruct(llvm_get_type(c, type), vals, count);
}

INLINE LLVMValueRef llvm_const_int(GenContext *c, Type *type, uint64_t val)
{
	type = type_lowering(type);
	ASSERT0(type_is_integer(type) || type->type_kind == TYPE_BOOL);
	return LLVMConstInt(llvm_get_type(c, type), val, type_is_integer_signed(type));
}

INLINE LLVMValueRef llvm_add_global(GenContext *c, const char *name, Type *type, AlignSize alignment)
{
	return llvm_add_global_raw(c, name, llvm_get_type(c, type_lowering(type_no_optional(type))), alignment);
}

INLINE LLVMValueRef llvm_add_global_raw(GenContext *c, const char *name, LLVMTypeRef type, AlignSize alignment)
{
	LLVMValueRef ref = LLVMAddGlobal(c->module, type, name);
	LLVMSetAlignment(ref, (unsigned)alignment ? alignment : LLVMPreferredAlignmentOfGlobal(c->target_data, ref));
	return ref;
}

INLINE void llvm_emit_exprid(GenContext *c, BEValue *value, ExprId expr)
{
	ASSERT0(expr);
	llvm_emit_expr(c, value, exprptr(expr));
}


INLINE void llvm_set_alignment(LLVMValueRef alloca, AlignSize alignment)
{
	ASSERT0(alignment > 0);
	LLVMSetAlignment(alloca, (unsigned)alignment);
}

INLINE void llvm_emit_statement_chain(GenContext *c, AstId current)
{
	while (current)
	{
		llvm_emit_stmt(c, ast_next(&current));
	}
}

