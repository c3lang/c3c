// Copyright (c) 2022-2025 Christoffer Lerno. All rights reserved.
// Use of this source code is governed by a LGPLv3.0
// a copy of which can be found in the LICENSE file.

#include "llvm_codegen_internal.h"

INLINE void llvm_emit_intrinsic_args(GenContext *c, Expr **args, LLVMValueRef *slots, unsigned count);

INLINE void llvm_emit_reverse(GenContext *c, BEValue *result_value, Expr *expr)
{
	Expr **args = expr->call_expr.arguments;
	llvm_emit_expr(c, result_value, args[0]);
	llvm_value_rvalue(c, result_value);
	Type *rtype = result_value->type;
	LLVMValueRef arg1 = result_value->value;
	LLVMValueRef arg2 = LLVMGetPoison(LLVMTypeOf(arg1));
	LLVMValueRef buff[128];
	unsigned elements = rtype->array.len;
	LLVMValueRef *mask_element = elements > 128 ? MALLOC(sizeof(LLVMValueRef)) : buff;
	LLVMTypeRef mask_element_type = llvm_get_type(c, type_int);
	for (unsigned i = 0; i < elements; i++)
	{
		mask_element[i] = LLVMConstInt(mask_element_type, elements - i - 1, false);
	}
	LLVMValueRef mask = LLVMConstVector(mask_element, elements);
	llvm_value_set(result_value, LLVMBuildShuffleVector(c->builder, arg1, arg2, mask, "reverse"), rtype);
}

INLINE void llvm_emit_select(GenContext *c, BEValue *result_value, Expr *expr)
{
	Expr **args = expr->call_expr.arguments;
	LLVMValueRef arg_slots[3];
	llvm_emit_intrinsic_args(c, args, arg_slots, 3);
	LLVMValueRef result = LLVMBuildSelect(c->builder, arg_slots[0], arg_slots[1], arg_slots[2], "select");
	llvm_value_set(result_value, result, expr->type);
}

INLINE void llvm_emit_swizzle(GenContext *c, BEValue *result_value, Expr *expr, bool swizzle_two)
{
	Expr **args = expr->call_expr.arguments;
	unsigned count = vec_size(args);
	LLVMValueRef arg2;
	llvm_emit_expr(c, result_value, args[0]);
	llvm_value_rvalue(c, result_value);
	LLVMValueRef arg1 = result_value->value;
	unsigned mask_start = 1;
	if (swizzle_two)
	{
		mask_start = 2;
		llvm_emit_expr(c, result_value, args[1]);
		llvm_value_rvalue(c, result_value);
		arg2 = result_value->value;
	}
	else
	{
		arg2 = LLVMGetPoison(LLVMTypeOf(arg1));
	}
#define MASK_VALS 256
	LLVMValueRef mask_cache[MASK_VALS];
	LLVMValueRef *mask_val = mask_cache;
	unsigned mask_len = count - mask_start;
	if (mask_len > MASK_VALS)
	{
		mask_val = MALLOC(sizeof(LLVMValueRef) * (mask_len));
	}
	for (unsigned i = mask_start; i < count; i++)
	{
		llvm_emit_expr(c, result_value, args[i]);
		llvm_value_rvalue(c, result_value);
		mask_val[i - mask_start] = result_value->value;
	}
	LLVMValueRef val = LLVMBuildShuffleVector(c->builder, arg1, arg2, LLVMConstVector(mask_val, mask_len), "shuffle");
	llvm_value_set(result_value, val, expr->type);
}

static LLVMAtomicOrdering ordering_to_llvm(int value)
{
	switch (value)
	{
		case ATOMIC_NONE: return LLVMAtomicOrderingNotAtomic;
		case ATOMIC_UNORDERED: return LLVMAtomicOrderingUnordered;
		case ATOMIC_RELAXED: return LLVMAtomicOrderingMonotonic;
		case ATOMIC_ACQUIRE: return LLVMAtomicOrderingAcquire;
		case ATOMIC_RELEASE: return LLVMAtomicOrderingRelease;
		case ATOMIC_ACQUIRE_RELEASE: return LLVMAtomicOrderingAcquireRelease;
		case ATOMIC_SEQ_CONSISTENT: return LLVMAtomicOrderingSequentiallyConsistent;
		default: UNREACHABLE;
	}
}

INLINE void llvm_emit_compare_exchange(GenContext *c, BEValue *result_value, Expr *expr)
{
	Expr **args = expr->call_expr.arguments;
	LLVMValueRef normal_args[3];
	BEValue value;
	for (int i = 0; i < 3; i++)
	{
		llvm_emit_expr(c, &value, args[i]);
		normal_args[i] = llvm_load_value_store(c, &value);
	}
	Type *type = value.type;
	bool is_volatile = args[3]->const_expr.b;
	bool is_weak = args[4]->const_expr.b;
	uint64_t success_ordering = args[5]->const_expr.ixx.i.low;
	uint64_t failure_ordering = args[6]->const_expr.ixx.i.low;
	uint64_t alignment = args[7]->const_expr.ixx.i.low;
	LLVMValueRef result = LLVMBuildAtomicCmpXchg(c->builder, normal_args[0], normal_args[1], normal_args[2],
												 ordering_to_llvm(success_ordering), ordering_to_llvm(failure_ordering), false);
	if (alignment && alignment >= type_abi_alignment(type))
	{
		ASSERT_SPAN(expr, is_power_of_two(alignment));
		LLVMSetAlignment(result, alignment);
	}
	if (is_volatile) LLVMSetVolatile(result, true);
	if (is_weak) LLVMSetWeak(result, true);

	llvm_value_set(result_value, llvm_emit_extract_value(c, result, 0), type);
}

INLINE void llvm_emit_unreachable_stmt(GenContext *c, BEValue *result_value)
{
	llvm_value_set(result_value, LLVMBuildUnreachable(c->builder), type_void);
	c->current_block = NULL;
}

INLINE void llvm_emit_volatile_store(GenContext *c, BEValue *result_value, Expr *expr)
{
	BEValue value;
	llvm_emit_expr(c, &value, expr->call_expr.arguments[0]);
	llvm_emit_expr(c, result_value, expr->call_expr.arguments[1]);
	llvm_value_deref(c, &value);
	BEValue store_value = *result_value;
	LLVMValueRef store = llvm_store(c, &value, &store_value);
	if (store) LLVMSetVolatile(store, true);
}

INLINE void llvm_emit_volatile_load(GenContext *c, BEValue *result_value, Expr *expr)
{
	llvm_emit_expr(c, result_value, expr->call_expr.arguments[0]);
	llvm_value_deref(c, result_value);
	llvm_value_rvalue(c, result_value);
	LLVMSetVolatile(result_value->value, true);
}

INLINE void llvm_emit_atomic_store(GenContext *c, BEValue *result_value, Expr *expr)
{
	BEValue value;
	llvm_emit_expr(c, &value, expr->call_expr.arguments[0]);
	llvm_emit_expr(c, result_value, expr->call_expr.arguments[1]);
	llvm_value_deref(c, &value);
	BEValue store_value = *result_value;
	LLVMValueRef store = llvm_store(c, &value, &store_value);
	if (store)
	{
		if (expr->call_expr.arguments[2]->const_expr.b) LLVMSetVolatile(store, true);
		LLVMSetOrdering(store, llvm_atomic_ordering(expr->call_expr.arguments[3]->const_expr.ixx.i.low));
	}
}

INLINE void llvm_emit_fence(GenContext *c, BEValue *result_value, Expr *expr)
{
	LLVMValueRef value = LLVMBuildFence(c->builder, llvm_atomic_ordering(expr->call_expr.arguments[0]->const_expr.ixx.i.low), compiler.build.single_threaded, "");
	llvm_value_set(result_value, value, type_void);
}

INLINE void llvm_emit_unaligned_store(GenContext *c, BEValue *result_value, Expr *expr)
{
	bool emit_check = c->emitting_load_store_check;
	c->emitting_load_store_check = true;
	BEValue value;
	llvm_emit_expr(c, &value, expr->call_expr.arguments[0]);
	llvm_value_rvalue(c, &value);
	llvm_emit_expr(c, result_value, expr->call_expr.arguments[1]);
	llvm_store_to_ptr_aligned(c, value.value, result_value, expr->call_expr.arguments[2]->const_expr.ixx.i.low);
	c->emitting_load_store_check = emit_check;
}

INLINE void llvm_emit_atomic_fetch(GenContext *c, BuiltinFunction func, BEValue *result_value, Expr *expr)
{
	BEValue value;
	llvm_emit_expr(c, &value, expr->call_expr.arguments[0]);
	llvm_emit_expr(c, result_value, expr->call_expr.arguments[1]);
	LLVMValueRef val = llvm_load_value_store(c, &value);
	bool is_float = type_is_float(result_value->type);
	bool is_unsigned = !is_float && type_is_unsigned(result_value->type);
	LLVMAtomicRMWBinOp op;
	static LLVMAtomicRMWBinOp LLVMAtomicRMWBinOpUIncWrap = LLVMAtomicRMWBinOpFMin + 1; // NOLINT
	static LLVMAtomicRMWBinOp LLVMAtomicRMWBinOpUDecWrap = LLVMAtomicRMWBinOpFMin + 2; // NOLINT
	switch (func)
	{
		case BUILTIN_ATOMIC_FETCH_EXCHANGE:
			op = LLVMAtomicRMWBinOpXchg;
			break;
		case BUILTIN_ATOMIC_FETCH_ADD:
			op = is_float ? LLVMAtomicRMWBinOpFAdd : LLVMAtomicRMWBinOpAdd;
			break;
		case BUILTIN_ATOMIC_FETCH_SUB:
			op = is_float ? LLVMAtomicRMWBinOpFSub : LLVMAtomicRMWBinOpSub;
			break;
		case BUILTIN_ATOMIC_FETCH_MAX:
			op = is_float ? LLVMAtomicRMWBinOpFMax : (is_unsigned ? LLVMAtomicRMWBinOpUMax : LLVMAtomicRMWBinOpMax);
			break;
		case BUILTIN_ATOMIC_FETCH_MIN:
			op = is_float ? LLVMAtomicRMWBinOpFMin : (is_unsigned ? LLVMAtomicRMWBinOpUMin : LLVMAtomicRMWBinOpMin);
			break;
		case BUILTIN_ATOMIC_FETCH_OR:
			op = LLVMAtomicRMWBinOpOr;
			break;
		case BUILTIN_ATOMIC_FETCH_XOR:
			op = LLVMAtomicRMWBinOpXor;
			break;
		case BUILTIN_ATOMIC_FETCH_NAND:
			op = LLVMAtomicRMWBinOpNand;
			break;
		case BUILTIN_ATOMIC_FETCH_AND:
			op = LLVMAtomicRMWBinOpAnd;
			break;
		case BUILTIN_ATOMIC_FETCH_INC_WRAP:
			op = LLVMAtomicRMWBinOpUIncWrap;
			break;
		case BUILTIN_ATOMIC_FETCH_DEC_WRAP:
			op = LLVMAtomicRMWBinOpUDecWrap;
			break;
		default:
			UNREACHABLE_VOID
	}
	LLVMValueRef res = LLVMBuildAtomicRMW(c->builder, op, val, llvm_load_value_store(c, result_value),
	                   llvm_atomic_ordering(expr->call_expr.arguments[3]->const_expr.ixx.i.low),
					   false);
	if (expr->call_expr.arguments[2]->const_expr.b) LLVMSetVolatile(res, true);
	uint64_t alignment = expr->call_expr.arguments[4]->const_expr.ixx.i.low;
	if (alignment)
	{
		ASSERT(is_power_of_two(alignment));
		LLVMSetAlignment(res, alignment);
	}
	llvm_value_set(result_value, res, result_value->type);
}

INLINE void llvm_emit_atomic_load(GenContext *c, BEValue *result_value, Expr *expr)
{
	llvm_emit_expr(c, result_value, expr->call_expr.arguments[0]);
	llvm_value_rvalue(c, result_value);
	Type *type = result_value->type->pointer;
	LLVMValueRef val = llvm_load(c, llvm_get_type(c, type), result_value->value, type_abi_alignment(type), "");
	if (expr->call_expr.arguments[1]->const_expr.b) LLVMSetVolatile(val, true);
	LLVMSetOrdering(val,  llvm_atomic_ordering(expr->call_expr.arguments[2]->const_expr.ixx.i.low));
	llvm_value_set(result_value, val, type);
}

INLINE void llvm_emit_unaligned_load(GenContext *c, BEValue *result_value, Expr *expr)
{
	bool emit_check = c->emitting_load_store_check;
	c->emitting_load_store_check = true;
	llvm_emit_expr(c, result_value, expr->call_expr.arguments[0]);
	llvm_value_deref(c, result_value);
	result_value->alignment = expr->call_expr.arguments[1]->const_expr.ixx.i.low;
	llvm_value_rvalue(c, result_value);
	c->emitting_load_store_check = emit_check;
}

static inline LLVMValueRef llvm_syscall_asm(LLVMTypeRef func_type, char *call)
{
	return LLVMGetInlineAsm(func_type, call, strlen(call),
							scratch_buffer_to_string(), scratch_buffer.len,
							true, true, LLVMInlineAsmDialectATT, /* can throw */ false);
}

static inline void llvm_syscall_write_regs_to_scratch(const char** registers, unsigned args)
{
	for (unsigned i = 0; i < args; i++)
	{
		scratch_buffer_append(",{");
		scratch_buffer_append(registers[i]);
		scratch_buffer_append("}");
	}
}

static inline void llvm_emit_syscall(GenContext *c, BEValue *be_value, Expr *expr)
{
	unsigned arguments = vec_size(expr->call_expr.arguments);
	ASSERT(arguments < 10 && "Only has room for 10");
	LLVMValueRef arg_results[10];
	LLVMTypeRef arg_types[10];
	Expr **args = expr->call_expr.arguments;
	LLVMTypeRef type = llvm_get_type(c, type_uptr);
	for (unsigned i = 0; i < arguments; i++)
	{
		llvm_emit_expr(c, be_value, args[i]);
		llvm_value_rvalue(c, be_value);
		arg_results[i] = be_value->value;
		arg_types[i] = type;
	}
	LLVMTypeRef func_type = LLVMFunctionType(type, arg_types, arguments, false);
	scratch_buffer_clear();
	LLVMValueRef inline_asm;
	switch (compiler.platform.arch)
	{
		case ARCH_TYPE_AARCH64:
		case ARCH_TYPE_AARCH64_BE:
			scratch_buffer_append("={x0}");
			ASSERT(arguments < 8);
			if (os_is_apple(compiler.platform.os))
			{
				static char const *regs[] = { "x16", "x0", "x1", "x2", "x3", "x4", "x5" };
				llvm_syscall_write_regs_to_scratch(regs, arguments);
			}
			else
			{
				static char const *regs[] = { "x8", "x0", "x1", "x2", "x3", "x4", "x5" };
				llvm_syscall_write_regs_to_scratch(regs, arguments);
			}
			inline_asm = llvm_syscall_asm(func_type, "svc #0x80");
			break;
		case ARCH_TYPE_X86:
		{
			scratch_buffer_append("={eax}");
			ASSERT(arguments < 8);
			static char const *regs[] = { "eax", "ebx", "ecx", "edx", "esi", "edi" };
			llvm_syscall_write_regs_to_scratch(regs, arguments < 6 ? arguments : 6);
			if (arguments == 7)
			{
				scratch_buffer_append(",rm");
				char *asm_str = "push %[arg6]\npush %%ebp\nmov 4(%%esp), %%ebp\nint $0x80\npop %%ebp\nadd $4, %%esp";
				inline_asm = llvm_syscall_asm(func_type, asm_str);
				break;
			}
			inline_asm = llvm_syscall_asm(func_type, "int $0x80");
			break;
		}
		case ARCH_TYPE_X86_64:
			scratch_buffer_append("={rax}");
			ASSERT(arguments < 8);
			{
				static char const *regs[] = { "rax", "rdi", "rsi", "rdx", "r10", "r8", "r9" };
				llvm_syscall_write_regs_to_scratch(regs, arguments);
			}
			// Check clobbers on different OSes
			scratch_buffer_append(",~{rcx},~{r11},~{memory}");
			inline_asm = llvm_syscall_asm(func_type, "syscall");
			break;
		case ARCH_UNSUPPORTED:
		default:
			UNREACHABLE_VOID
	}
	LLVMValueRef result = LLVMBuildCall2(c->builder, func_type, inline_asm, arg_results, arguments, "syscall");
	llvm_value_set(be_value, result, type_uptr);
}

INLINE unsigned llvm_intrinsic_by_type(Type *type, unsigned int_intrinsic, unsigned uint_intrinsic, unsigned float_intrinsic)
{
	type = type_flatten(type);
	RETRY:
	switch (type->type_kind)
	{
		case ALL_SIGNED_INTS:
			return int_intrinsic;
		case TYPE_BOOL:
		case ALL_UNSIGNED_INTS:
			return uint_intrinsic;
		case ALL_FLOATS:
			return float_intrinsic;
		case VECTORS:
			type = type->array.base;
			goto RETRY;
		default:
			UNREACHABLE
	}
}

INLINE void llvm_emit_intrinsic_args(GenContext *c, Expr **args, LLVMValueRef *slots, unsigned count)
{
	BEValue be_value;
	for (unsigned i = 0; i < count; i++)
	{
		llvm_emit_expr(c, &be_value, args[i]);
		llvm_value_rvalue(c, &be_value);
		slots[i] = be_value.value;
	}
}

INLINE void llvm_emit_memcpy_builtin(GenContext *c, unsigned intrinsic, BEValue *be_value, Expr *expr)
{
	Expr **args = expr->call_expr.arguments;
	LLVMValueRef arg_slots[4];
	llvm_emit_intrinsic_args(c, args, arg_slots, 4);
	LLVMTypeRef call_type[3];
	call_type[0] = call_type[1] = c->ptr_type;
	call_type[2] = c->size_type;
	LLVMValueRef result = llvm_emit_call_intrinsic(c, intrinsic, call_type, 3, arg_slots, 4);
	ASSERT(args[4]->const_expr.const_kind == CONST_INTEGER);
	ASSERT(args[5]->const_expr.const_kind == CONST_INTEGER);
	uint64_t dst_align = int_to_u64(args[4]->const_expr.ixx);
	uint64_t src_align = int_to_u64(args[5]->const_expr.ixx);
	if (dst_align > 0) llvm_attribute_add_call(c, result, attribute_id.align, 1, dst_align);
	if (src_align > 0) llvm_attribute_add_call(c, result, attribute_id.align, 2, src_align);
	llvm_value_set(be_value, result, type_void);
}

INLINE void llvm_emit_memmove_builtin(GenContext *c, BEValue *be_value, Expr *expr)
{
	Expr **args = expr->call_expr.arguments;
	LLVMValueRef arg_slots[4];
	llvm_emit_intrinsic_args(c, args, arg_slots, 4);
	LLVMTypeRef call_type[3];
	call_type[0] = call_type[1] = c->ptr_type;
	call_type[2] = c->size_type;
	LLVMValueRef result = llvm_emit_call_intrinsic(c, intrinsic_id.memmove, call_type, 3, arg_slots, 4);
	ASSERT(args[4]->const_expr.const_kind == CONST_INTEGER);
	ASSERT(args[5]->const_expr.const_kind == CONST_INTEGER);
	uint64_t dst_align = int_to_u64(args[4]->const_expr.ixx);
	uint64_t src_align = int_to_u64(args[5]->const_expr.ixx);
	if (dst_align > 0) llvm_attribute_add_call(c, result, attribute_id.align, 1, dst_align);
	if (src_align > 0) llvm_attribute_add_call(c, result, attribute_id.align, 2, src_align);
	llvm_value_set(be_value, result, type_void);
}

INLINE void llvm_emit_memset_builtin(GenContext *c, unsigned intrinsic, BEValue *be_value, Expr *expr)
{
	Expr **args = expr->call_expr.arguments;
	LLVMValueRef arg_slots[4];
	llvm_emit_intrinsic_args(c, args, arg_slots, 4);
	LLVMTypeRef call_type[2] = { c->ptr_type, c->size_type };
	LLVMValueRef result = llvm_emit_call_intrinsic(c, intrinsic, call_type, 2, arg_slots, 4);
	ASSERT(args[4]->const_expr.const_kind == CONST_INTEGER);
	uint64_t dst_align = int_to_u64(args[4]->const_expr.ixx);
	if (dst_align > 0) llvm_attribute_add_call(c, result, attribute_id.align, 1, dst_align);
	llvm_value_set(be_value, result, type_void);
}

INLINE void llvm_emit_prefetch(GenContext *c, BEValue *be_value, Expr *expr)
{
	Expr **args = expr->call_expr.arguments;
	LLVMValueRef arg_slots[4];
	llvm_emit_intrinsic_args(c, args, arg_slots, 3);
	arg_slots[3] = llvm_const_int(c, type_int, 1);
	LLVMTypeRef call_type[1] = { c->ptr_type };
	LLVMValueRef result = llvm_emit_call_intrinsic(c, intrinsic_id.prefetch, call_type, 1, arg_slots, 4);
	llvm_value_set(be_value, result, type_void);
}

static void llvm_emit_reduce_int_builtin(GenContext *c, unsigned intrinsic, BEValue *be_value, Expr *expr)
{
	Expr **args = expr->call_expr.arguments;
	LLVMValueRef arg_slots[1];
	llvm_emit_intrinsic_args(c, args, arg_slots, 1);
	LLVMTypeRef call_type[1] = { LLVMTypeOf(arg_slots[0]) };
	LLVMValueRef result = llvm_emit_call_intrinsic(c, intrinsic, call_type, 1, arg_slots, 1);
	llvm_value_set(be_value, result, expr->type);
}

void llvm_emit_reduce_float_builtin(GenContext *c, unsigned intrinsic, BEValue *be_value, Expr *expr)
{
	Expr **args = expr->call_expr.arguments;
	LLVMValueRef arg_slots[2];
	llvm_emit_intrinsic_args(c, args, arg_slots, 2);
	LLVMTypeRef call_type[1] = { LLVMTypeOf(arg_slots[1]) };
	LLVMValueRef result = llvm_emit_call_intrinsic(c, intrinsic, call_type, 1, arg_slots, 2);
	llvm_value_set(be_value, result, expr->type);
}

void llvm_emit_int_with_bool_builtin(GenContext *c, unsigned intrinsic, BEValue *be_value, Expr *expr, bool bool_val)
{
	Expr **args = expr->call_expr.arguments;
	LLVMValueRef arg_slots[2];
	llvm_emit_intrinsic_args(c, args, arg_slots, 1);
	arg_slots[1] = LLVMConstInt(c->bool_type, bool_val ? 1 : 0, false);
	LLVMTypeRef call_type[1] = { LLVMTypeOf(arg_slots[0]) };
	LLVMValueRef result = llvm_emit_call_intrinsic(c, intrinsic, call_type, 1, arg_slots, 2);
	llvm_value_set(be_value, result, expr->type);
}

void llvm_emit_pow_int_builtin(GenContext *c, BEValue *be_value, Expr *expr)
{
	Expr **args = expr->call_expr.arguments;
	LLVMValueRef arg_slots[2];
	llvm_emit_intrinsic_args(c, args, arg_slots, 2);
	LLVMTypeRef call_type[2] = { LLVMTypeOf(arg_slots[0]), LLVMTypeOf(arg_slots[1]) };
	LLVMValueRef result = llvm_emit_call_intrinsic(c, intrinsic_id.powi, call_type, 2, arg_slots, 2);
	llvm_value_set(be_value, result, expr->type);
}

void llvm_emit_3_variant_builtin(GenContext *c, BEValue *be_value, Expr *expr, unsigned sid, unsigned uid, unsigned fid)
{
	Expr **args = expr->call_expr.arguments;
	unsigned count = vec_size(args);
	ASSERT(count <= 3);
	LLVMValueRef arg_slots[3];
	unsigned intrinsic = llvm_intrinsic_by_type(args[0]->type, sid, uid, fid);
	llvm_emit_intrinsic_args(c, args, arg_slots, count);
	if (count == 2 && (intrinsic == intrinsic_id.smul_fixed_sat || intrinsic == intrinsic_id.umul_fixed_sat))
	{
		arg_slots[2] = llvm_const_int(c, type_int, 0);
		count++;
	}
	LLVMTypeRef call_type[1] = { LLVMTypeOf(arg_slots[0]) }; // NOLINT
	LLVMValueRef result = llvm_emit_call_intrinsic(c, intrinsic, call_type, 1, arg_slots, count);
	llvm_value_set(be_value, result, expr->type);
}

void llvm_emit_abs_builtin(GenContext *c, BEValue *be_value, Expr *expr)
{
	Expr **args = expr->call_expr.arguments;
	LLVMValueRef arg_slots[2];
	llvm_emit_intrinsic_args(c, args, arg_slots, 1);
	unsigned intrinsic = llvm_intrinsic_by_type(args[0]->type, intrinsic_id.abs, intrinsic_id.abs, intrinsic_id.fabs);
	LLVMTypeRef call_type[1] = { LLVMTypeOf(arg_slots[0]) };
	LLVMValueRef result;
	if (intrinsic == intrinsic_id.abs)
	{
		arg_slots[1] = llvm_get_zero_raw(c->bool_type);
		result = llvm_emit_call_intrinsic(c, intrinsic, call_type, 1, arg_slots, 2);
	}
	else
	{
		result = llvm_emit_call_intrinsic(c, intrinsic, call_type, 1, arg_slots, 1);
	}
	llvm_value_set(be_value, result, expr->type);
}

void llvm_emit_simple_builtin(GenContext *c, BEValue *be_value, Expr *expr, unsigned intrinsic)
{
	Expr **args = expr->call_expr.arguments;
	unsigned count = vec_size(args);
	ASSERT(count <= 4);
	ASSERT(count > 0);
	LLVMValueRef arg_slots[4];
	llvm_emit_intrinsic_args(c, args, arg_slots, count);
	LLVMTypeRef call_type = LLVMTypeOf(arg_slots[0]);
	LLVMValueRef result = llvm_emit_call_intrinsic(c, intrinsic, &call_type, 1, arg_slots, count);
	llvm_value_set(be_value, result, expr->type);
}

void llvm_emit_matrix_multiply(GenContext *c, BEValue *be_value, Expr *expr)
{
	Expr **args = expr->call_expr.arguments;
	unsigned count = vec_size(args);
	ASSERT(count == 5);
	LLVMValueRef arg_slots[5];
	llvm_emit_intrinsic_args(c, args, arg_slots, count);
	LLVMTypeRef type = LLVMTypeOf(arg_slots[0]);
	LLVMTypeRef result_type = llvm_get_type(c, expr->type);
	LLVMTypeRef call_type[3] = { result_type, type,  LLVMTypeOf(arg_slots[1]) };
	LLVMValueRef result = llvm_emit_call_intrinsic(c, intrinsic_id.matrix_multiply, call_type, 3, arg_slots, count);
	llvm_value_set(be_value, result, expr->type);
}

void llvm_emit_matrix_transpose(GenContext *c, BEValue *be_value, Expr *expr)
{
	Expr **args = expr->call_expr.arguments;
	unsigned count = vec_size(args);
	ASSERT(count == 3);
	LLVMValueRef arg_slots[3];
	llvm_emit_intrinsic_args(c, args, arg_slots, count);
	LLVMTypeRef type = LLVMTypeOf(arg_slots[0]);
	LLVMTypeRef result_type = llvm_get_type(c, expr->type);
	LLVMTypeRef call_type[3] = { result_type, type };
	LLVMValueRef result = llvm_emit_call_intrinsic(c, intrinsic_id.matrix_transpose, call_type, 2, arg_slots, count);
	llvm_value_set(be_value, result, expr->type);
}

static void llvm_emit_masked_load(GenContext *c, BEValue *be_value, Expr *expr)
{
	Expr **args = expr->call_expr.arguments;
	ASSERT(vec_size(args) == 4);
	LLVMValueRef arg_slots[4];
	llvm_emit_intrinsic_args(c, args, arg_slots, 3);
	// Rearrange to match our builtin with the intrinsic which is ptr, align, mask, passthru
	LLVMValueRef passthru = arg_slots[2];
	LLVMValueRef mask = arg_slots[1];
	uint64_t alignment = args[3]->const_expr.ixx.i.low;
	LLVMTypeRef call_type[2] = { LLVMTypeOf(passthru), c->ptr_type };
	arg_slots[1] = llvm_const_int(c, type_uint, alignment ? alignment : llvm_abi_alignment(c, call_type[0]));
	arg_slots[2] = mask;
	arg_slots[3] = passthru;
	LLVMValueRef result = llvm_emit_call_intrinsic(c, intrinsic_id.masked_load, call_type, 2, arg_slots, 4);
	llvm_value_set(be_value, result, expr->type);
}

static void llvm_emit_gather(GenContext *c, BEValue *be_value, Expr *expr)
{
	Expr **args = expr->call_expr.arguments;
	ASSERT(vec_size(args) == 4);
	LLVMValueRef arg_slots[4];
	llvm_emit_intrinsic_args(c, args, arg_slots, 3);
	// Rearrange to match our builtin with the intrinsic which is ptr, align, mask, passthru
	LLVMValueRef passthru = arg_slots[2];
	LLVMValueRef mask = arg_slots[1];
	uint64_t alignment = args[3]->const_expr.ixx.i.low;
	LLVMTypeRef call_type[2] = { LLVMTypeOf(passthru), LLVMTypeOf(arg_slots[0]) };
	arg_slots[1] = llvm_const_int(c, type_uint, alignment ? alignment : llvm_abi_alignment(c, LLVMGetElementType(call_type[0])));
	arg_slots[2] = mask;
	arg_slots[3] = passthru;
	LLVMValueRef result = llvm_emit_call_intrinsic(c, intrinsic_id.gather, call_type, 2, arg_slots, 4);
	llvm_value_set(be_value, result, expr->type);
}

static void llvm_emit_masked_store(GenContext *c, BEValue *be_value, Expr *expr)
{
	Expr **args = expr->call_expr.arguments;
	ASSERT(vec_size(args) == 4);
	LLVMValueRef arg_slots[4];
	llvm_emit_intrinsic_args(c, args, arg_slots, 3);
	// Rearrange to match our builtin with the intrinsic which is value, ptr, align, mask
	LLVMValueRef ptr = arg_slots[0];
	LLVMValueRef value = arg_slots[1];
	LLVMValueRef mask = arg_slots[2];
	arg_slots[0] = value;
	arg_slots[1] = ptr;
	uint64_t alignment = args[3]->const_expr.ixx.i.low;
	LLVMTypeRef call_type[2] = { LLVMTypeOf(value), c->ptr_type };
	arg_slots[2] = llvm_const_int(c, type_uint, alignment ? alignment : llvm_abi_alignment(c, call_type[0]));
	arg_slots[3] = mask;
	LLVMValueRef result = llvm_emit_call_intrinsic(c, intrinsic_id.masked_store, call_type, 2, arg_slots, 4);
	llvm_value_set(be_value, result, expr->type);
}

static void llvm_emit_scatter(GenContext *c, BEValue *be_value, Expr *expr)
{
	Expr **args = expr->call_expr.arguments;
	ASSERT(vec_size(args) == 4);
	LLVMValueRef arg_slots[4];
	llvm_emit_intrinsic_args(c, args, arg_slots, 3);
	// Rearrange to match our builtin with the intrinsic which is value, ptr, align, mask
	LLVMValueRef ptr = arg_slots[0];
	LLVMValueRef value = arg_slots[1];
	LLVMValueRef mask = arg_slots[2];
	arg_slots[0] = value;
	arg_slots[1] = ptr;
	uint64_t alignment = args[3]->const_expr.ixx.i.low;
	LLVMTypeRef call_type[2] = { LLVMTypeOf(value),  LLVMTypeOf(ptr) };
	arg_slots[2] = llvm_const_int(c, type_uint, alignment ? alignment : llvm_abi_alignment(c, LLVMGetElementType(call_type[0])));
	arg_slots[3] = mask;
	LLVMValueRef result = llvm_emit_call_intrinsic(c, intrinsic_id.scatter, call_type, 2, arg_slots, 4);
	llvm_value_set(be_value, result, expr->type);
}


void llvm_emit_builtin_args_types3(GenContext *c, BEValue *be_value, Expr *expr, unsigned intrinsic, Type *type1, Type *type2, Type *type3)
{
	Expr **args = expr->call_expr.arguments;
	unsigned count = vec_size(args);
	ASSERT(count <= 3);
	ASSERT(count > 0);
	LLVMValueRef arg_slots[3];
	llvm_emit_intrinsic_args(c, args, arg_slots, count);
	LLVMTypeRef call_type[3];
	unsigned type_slots = 0;
	if (type1) call_type[type_slots++] = llvm_get_type(c, type1);
	if (type2) call_type[type_slots++] = llvm_get_type(c, type2);
	if (type3) call_type[type_slots++] = llvm_get_type(c, type3);
	LLVMValueRef result = llvm_emit_call_intrinsic(c, intrinsic, call_type, type_slots, arg_slots, count);
	llvm_value_set(be_value, result, expr->type);
}

static void llvm_emit_overflow_builtin(GenContext *c, BEValue *be_value, Expr *expr, unsigned intrinsic_signed, unsigned intrinsic_unsigned)
{
	Expr **args = expr->call_expr.arguments;
	LLVMValueRef arg_slots[2];
	llvm_emit_intrinsic_args(c, args, arg_slots, 2);
	BEValue ref;
	Expr *ret_addr = args[2];
	llvm_emit_expr(c, &ref, ret_addr);
	llvm_value_rvalue(c, &ref);
	// Note that we can make additional improvements here!
	llvm_value_set_address(c, &ref, ref.value, ref.type->pointer, type_abi_alignment(ref.type->pointer));
	LLVMTypeRef call_type[1] = { LLVMTypeOf(arg_slots[0]) };
	unsigned intrinsic = type_is_signed(type_lowering(args[0]->type)) ? intrinsic_signed : intrinsic_unsigned;
	LLVMValueRef result = llvm_emit_call_intrinsic(c, intrinsic, call_type, 1, arg_slots, 2);
	LLVMValueRef failed = llvm_emit_extract_value(c, result, 1);
	LLVMValueRef value = llvm_emit_extract_value(c, result, 0);
	llvm_store_raw(c, &ref, value);
	llvm_value_set(be_value, failed, type_bool);
}

static void llvm_emit_wrap_builtin(GenContext *c, BEValue *result_value, Expr *expr, BuiltinFunction func)
{
	Expr **args = expr->call_expr.arguments;
	LLVMValueRef arg_slots[2];
	llvm_emit_intrinsic_args(c, args, arg_slots, func == BUILTIN_EXACT_NEG ? 1 : 2);
	Type *base_type = type_lowering(args[0]->type);
	if (type_kind_is_real_vector(base_type->type_kind)) base_type = base_type->array.base;
	ASSERT(type_is_integer(base_type));
	LLVMValueRef res;
	switch (func)
	{
		case BUILTIN_EXACT_NEG:
			res = LLVMBuildNeg(c->builder, arg_slots[0], "eneg");
			break;
		case BUILTIN_EXACT_SUB:
			res = LLVMBuildSub(c->builder, arg_slots[0], arg_slots[1], "esub");
			break;
		case BUILTIN_EXACT_ADD:
			res = LLVMBuildAdd(c->builder, arg_slots[0], arg_slots[1], "eadd");
			break;
		case BUILTIN_EXACT_MUL:
			res = LLVMBuildMul(c->builder, arg_slots[0], arg_slots[1], "emul");
			break;
		case BUILTIN_EXACT_DIV:
			if (type_is_signed(base_type))
			{
				res = LLVMBuildSDiv(c->builder, arg_slots[0], arg_slots[1], "esdiv");
			}
			else
			{
				res = LLVMBuildUDiv(c->builder, arg_slots[0], arg_slots[1], "eudiv");
			}
			break;
		case BUILTIN_EXACT_MOD:
			if (type_is_signed(base_type))
			{
				res = LLVMBuildSRem(c->builder, arg_slots[0], arg_slots[1], "eumod");
			}
			else
			{
				res = LLVMBuildSDiv(c->builder, arg_slots[0], arg_slots[1], "esmod");
			}
			break;
		default:
			UNREACHABLE_VOID
	}
	llvm_value_set(result_value, res, expr->type);
}


void llvm_emit_builtin_call(GenContext *c, BEValue *result_value, Expr *expr)
{
	BuiltinFunction func = exprptr(expr->call_expr.function)->builtin_expr.builtin;
	switch (func)
	{
		case BUILTIN_ANY_MAKE:
			// Folded in the frontend.
			UNREACHABLE_VOID
		case BUILTIN_UNREACHABLE:
			llvm_emit_unreachable_stmt(c, result_value);
			return;
		case BUILTIN_SWIZZLE:
			llvm_emit_swizzle(c, result_value, expr, false);
			return;
		case BUILTIN_SWIZZLE2:
			llvm_emit_swizzle(c, result_value, expr, true);
			return;
		case BUILTIN_COMPARE_EXCHANGE:
			llvm_emit_compare_exchange(c, result_value, expr);
			return;
		case BUILTIN_FRAMEADDRESS:
			{
				llvm_emit_expr(c, result_value, expr->call_expr.arguments[0]);
				llvm_value_rvalue(c, result_value);
				LLVMValueRef res = llvm_emit_call_intrinsic(c, intrinsic_id.frameaddress, &c->ptr_type, 1, &result_value->value, 1);
				llvm_value_set(result_value, res, expr->type);
				return;
			}
		case BUILTIN_RETURNADDRESS:
		{
			llvm_emit_expr(c, result_value, expr->call_expr.arguments[0]);
			llvm_value_rvalue(c, result_value);
			LLVMValueRef res = llvm_emit_call_intrinsic(c, intrinsic_id.returnaddress, NULL, 0, &result_value->value, 1);
			llvm_value_set(result_value, res, expr->type);
			return;
		}
		case BUILTIN_SELECT:
			llvm_emit_select(c, result_value, expr);
			return;
		case BUILTIN_VECCOMPLT:
		case BUILTIN_VECCOMPLE:
		case BUILTIN_VECCOMPNE:
		case BUILTIN_VECCOMPEQ:
		case BUILTIN_VECCOMPGT:
		case BUILTIN_VECCOMPGE:
			UNREACHABLE_VOID
		case BUILTIN_REVERSE:
			llvm_emit_reverse(c, result_value, expr);
			return;
		case BUILTIN_VOLATILE_STORE:
			llvm_emit_volatile_store(c, result_value, expr);
			return;
		case BUILTIN_VOLATILE_LOAD:
			llvm_emit_volatile_load(c, result_value, expr);
			return;
		case BUILTIN_FENCE:
			llvm_emit_fence(c, result_value, expr);
			return;
		case BUILTIN_ATOMIC_STORE:
			llvm_emit_atomic_store(c, result_value, expr);
			return;
		case BUILTIN_ATOMIC_FETCH_ADD:
		case BUILTIN_ATOMIC_FETCH_INC_WRAP:
		case BUILTIN_ATOMIC_FETCH_NAND:
		case BUILTIN_ATOMIC_FETCH_AND:
		case BUILTIN_ATOMIC_FETCH_OR:
		case BUILTIN_ATOMIC_FETCH_XOR:
		case BUILTIN_ATOMIC_FETCH_MAX:
		case BUILTIN_ATOMIC_FETCH_MIN:
		case BUILTIN_ATOMIC_FETCH_SUB:
		case BUILTIN_ATOMIC_FETCH_DEC_WRAP:
		case BUILTIN_ATOMIC_FETCH_EXCHANGE:
			llvm_emit_atomic_fetch(c, func, result_value, expr);
			return;
		case BUILTIN_UNALIGNED_LOAD:
			llvm_emit_unaligned_load(c, result_value, expr);
			return;
		case BUILTIN_UNALIGNED_STORE:
			llvm_emit_unaligned_store(c, result_value, expr);
			return;
		case BUILTIN_ATOMIC_LOAD:
			llvm_emit_atomic_load(c, result_value, expr);
			return;
		case BUILTIN_SYSCALL:
			llvm_emit_syscall(c, result_value, expr);
			return;
		case BUILTIN_MEMCOPY:
			llvm_emit_memcpy_builtin(c, intrinsic_id.memcpy, result_value, expr);
			return;
		case BUILTIN_MEMCOPY_INLINE:
			llvm_emit_memcpy_builtin(c, intrinsic_id.memcpy_inline, result_value, expr);
			return;
		case BUILTIN_MEMMOVE:
			llvm_emit_memmove_builtin(c, result_value, expr);
			return;
		case BUILTIN_MEMSET:
			llvm_emit_memset_builtin(c, intrinsic_id.memset, result_value, expr);
			return;
		case BUILTIN_MEMSET_INLINE:
			llvm_emit_memset_builtin(c, intrinsic_id.memset_inline, result_value, expr);
			return;
		case BUILTIN_MATRIX_MUL:
			llvm_emit_matrix_multiply(c, result_value, expr);
			return;
		case BUILTIN_MATRIX_TRANSPOSE:
			compiler.linking.link_math = true;
			llvm_emit_simple_builtin(c, result_value, expr, intrinsic_id.matrix_transpose);
			return;
		case BUILTIN_SYSCLOCK:
			llvm_value_set(result_value, llvm_emit_call_intrinsic(c, intrinsic_id.readcyclecounter, NULL, 0, NULL, 0), expr->type);
			return;
		case BUILTIN_TRAP:
			llvm_value_set(result_value, llvm_emit_call_intrinsic(c, intrinsic_id.trap, NULL, 0, NULL, 0), type_void);
			return;
		case BUILTIN_BREAKPOINT:
			llvm_value_set(result_value, llvm_emit_call_intrinsic(c, intrinsic_id.debugtrap, NULL, 0, NULL, 0), type_void);
			return;
		case BUILTIN_PREFETCH:
			llvm_emit_prefetch(c, result_value, expr);
			return;
		case BUILTIN_REDUCE_AND:
			compiler.linking.link_math = true;
			llvm_emit_reduce_int_builtin(c, intrinsic_id.vector_reduce_and, result_value, expr);
			return;
		case BUILTIN_REDUCE_OR:
			compiler.linking.link_math = true;
			llvm_emit_reduce_int_builtin(c, intrinsic_id.vector_reduce_or, result_value, expr);
			return;
		case BUILTIN_REDUCE_MIN:
			compiler.linking.link_math = true;
			llvm_emit_3_variant_builtin(c, result_value, expr, intrinsic_id.vector_reduce_smin, intrinsic_id.vector_reduce_umin, intrinsic_id.vector_reduce_fmin);
			return;
		case BUILTIN_REDUCE_MAX:
			compiler.linking.link_math = true;
			llvm_emit_3_variant_builtin(c, result_value, expr, intrinsic_id.vector_reduce_smax, intrinsic_id.vector_reduce_umax, intrinsic_id.vector_reduce_fmax);
			return;
		case BUILTIN_REDUCE_XOR:
			compiler.linking.link_math = true;
			llvm_emit_reduce_int_builtin(c, intrinsic_id.vector_reduce_xor, result_value, expr);
			return;
		case BUILTIN_REDUCE_ADD:
			compiler.linking.link_math = true;
			llvm_emit_reduce_int_builtin(c, intrinsic_id.vector_reduce_add, result_value, expr);
			return;
		case BUILTIN_REDUCE_MUL:
			compiler.linking.link_math = true;
			llvm_emit_reduce_int_builtin(c, intrinsic_id.vector_reduce_mul, result_value, expr);
			return;
		case BUILTIN_REDUCE_FADD:
			compiler.linking.link_math = true;
			llvm_emit_reduce_float_builtin(c, intrinsic_id.vector_reduce_fadd, result_value, expr);
			return;
		case BUILTIN_REDUCE_FMUL:
			compiler.linking.link_math = true;
			llvm_emit_reduce_float_builtin(c, intrinsic_id.vector_reduce_fmul, result_value, expr);
			return;
		case BUILTIN_EXACT_DIV:
		case BUILTIN_EXACT_ADD:
		case BUILTIN_EXACT_MUL:
		case BUILTIN_EXACT_SUB:
		case BUILTIN_EXACT_MOD:
		case BUILTIN_EXACT_NEG:
			compiler.linking.link_math = true;
			llvm_emit_wrap_builtin(c, result_value, expr, func);
			return;
		case BUILTIN_OVERFLOW_ADD:
			compiler.linking.link_math = true;
			llvm_emit_overflow_builtin(c, result_value, expr, intrinsic_id.sadd_overflow, intrinsic_id.uadd_overflow);
			return;
		case BUILTIN_OVERFLOW_SUB:
			compiler.linking.link_math = true;
			llvm_emit_overflow_builtin(c, result_value, expr, intrinsic_id.ssub_overflow, intrinsic_id.usub_overflow);
			return;
		case BUILTIN_OVERFLOW_MUL:
			compiler.linking.link_math = true;
			llvm_emit_overflow_builtin(c, result_value, expr, intrinsic_id.smul_overflow, intrinsic_id.umul_overflow);
			return;
		case BUILTIN_CTTZ:
			llvm_emit_int_with_bool_builtin(c, intrinsic_id.cttz, result_value, expr, false);
			return;
		case BUILTIN_CTLZ:
			llvm_emit_int_with_bool_builtin(c, intrinsic_id.ctlz, result_value, expr, false);
			return;
		case BUILTIN_EXPECT:
			llvm_emit_simple_builtin(c, result_value, expr, intrinsic_id.expect);
			return;
		case BUILTIN_GATHER:
			llvm_emit_gather(c, result_value, expr);
			return;
		case BUILTIN_SCATTER:
			llvm_emit_scatter(c, result_value, expr);
			return;
		case BUILTIN_MASKED_STORE:
			llvm_emit_masked_store(c, result_value, expr);
			return;
		case BUILTIN_MASKED_LOAD:
			llvm_emit_masked_load(c, result_value, expr);
			return;
		case BUILTIN_EXPECT_WITH_PROBABILITY:
			if (compiler.build.optlevel == OPTIMIZATION_NONE)
			{
				Expr **args = expr->call_expr.arguments;
				llvm_emit_expr(c, result_value, args[0]);
				BEValue dummy;
				llvm_emit_expr(c, &dummy, args[1]);
				return;
			}
			llvm_emit_simple_builtin(c, result_value, expr, intrinsic_id.expect_with_probability);
			return;
		case BUILTIN_MAX:
			llvm_emit_3_variant_builtin(c, result_value, expr, intrinsic_id.smax, intrinsic_id.umax, intrinsic_id.maxnum);
			return;
		case BUILTIN_MIN:
			llvm_emit_3_variant_builtin(c, result_value, expr, intrinsic_id.smin, intrinsic_id.umin, intrinsic_id.minnum);
			return;
		case BUILTIN_SAT_SHL:
			compiler.linking.link_math = true;
			llvm_emit_3_variant_builtin(c, result_value, expr, intrinsic_id.sshl_sat, intrinsic_id.ushl_sat, 0);
			return;
		case BUILTIN_SAT_ADD:
			compiler.linking.link_math = true;
			llvm_emit_3_variant_builtin(c, result_value, expr, intrinsic_id.sadd_sat, intrinsic_id.uadd_sat, 0);
			return;
		case BUILTIN_SAT_SUB:
			compiler.linking.link_math = true;
			llvm_emit_3_variant_builtin(c, result_value, expr, intrinsic_id.ssub_sat, intrinsic_id.usub_sat, 0);
			return;
		case BUILTIN_SAT_MUL:
			compiler.linking.link_math = true;
			llvm_emit_3_variant_builtin(c, result_value, expr, intrinsic_id.smul_fixed_sat, intrinsic_id.umul_fixed_sat, 0);
			return;
		case BUILTIN_ABS:
			compiler.linking.link_math = true;
			llvm_emit_abs_builtin(c, result_value, expr);
			return;
		case BUILTIN_POW_INT:
			compiler.linking.link_math = true;
			llvm_emit_pow_int_builtin(c, result_value, expr);
			return;
		case BUILTIN_BITREVERSE:
			llvm_emit_simple_builtin(c, result_value, expr, intrinsic_id.bitreverse);
			return;
		case BUILTIN_BSWAP:
			llvm_emit_simple_builtin(c, result_value, expr, intrinsic_id.bswap);
			return;
		case BUILTIN_CEIL:
			llvm_emit_simple_builtin(c, result_value, expr, intrinsic_id.ceil);
			return;
		case BUILTIN_COS:
			compiler.linking.link_math = true;
			llvm_emit_simple_builtin(c, result_value, expr, intrinsic_id.cos);
			return;
		case BUILTIN_COPYSIGN:
			compiler.linking.link_math = true;
			llvm_emit_simple_builtin(c, result_value, expr, intrinsic_id.copysign);
			return;
		case BUILTIN_FLOOR:
			compiler.linking.link_math = true;
			llvm_emit_simple_builtin(c, result_value, expr, intrinsic_id.floor);
			return;
		case BUILTIN_EXP:
			compiler.linking.link_math = true;
			llvm_emit_simple_builtin(c, result_value, expr, intrinsic_id.exp);
			return;
		case BUILTIN_EXP2:
			compiler.linking.link_math = true;
			llvm_emit_simple_builtin(c, result_value, expr, intrinsic_id.exp2);
			return;
		case BUILTIN_FMA:
			compiler.linking.link_math = true;
			llvm_emit_simple_builtin(c, result_value, expr, intrinsic_id.fma);
			return;
		case BUILTIN_FMULADD:
			compiler.linking.link_math = true;
			llvm_emit_simple_builtin(c, result_value, expr, intrinsic_id.fmuladd);
			return;
		case BUILTIN_FSHL:
			llvm_emit_simple_builtin(c, result_value, expr, intrinsic_id.fshl);
			return;
		case BUILTIN_FSHR:
			llvm_emit_simple_builtin(c, result_value, expr, intrinsic_id.fshr);
			return;
		case BUILTIN_GET_ROUNDING_MODE:
			compiler.linking.link_math = true;
			llvm_value_set(result_value, llvm_emit_call_intrinsic(c, intrinsic_id.get_rounding, NULL, 0, NULL, 0), expr->type);
			return;
		case BUILTIN_LOG:
			compiler.linking.link_math = true;
			llvm_emit_simple_builtin(c, result_value, expr, intrinsic_id.log);
			return;
		case BUILTIN_LOG2:
			compiler.linking.link_math = true;
			llvm_emit_simple_builtin(c, result_value, expr, intrinsic_id.log2);
			return;
		case BUILTIN_LOG10:
			compiler.linking.link_math = true;
			llvm_emit_simple_builtin(c, result_value, expr, intrinsic_id.log10);
			return;
		case BUILTIN_POW:
			compiler.linking.link_math = true;
			llvm_emit_simple_builtin(c, result_value, expr, intrinsic_id.pow);
			return;
		case BUILTIN_NEARBYINT:
			compiler.linking.link_math = true;
			llvm_emit_simple_builtin(c, result_value, expr, intrinsic_id.nearbyint);
			return;
		case BUILTIN_POPCOUNT:
			llvm_emit_simple_builtin(c, result_value, expr, intrinsic_id.ctpop);
			return;
		case BUILTIN_RINT:
			compiler.linking.link_math = true;
			llvm_emit_simple_builtin(c, result_value, expr, intrinsic_id.rint);
			return;
		case BUILTIN_ROUND:
			compiler.linking.link_math = true;
			llvm_emit_simple_builtin(c, result_value, expr, intrinsic_id.round);
			return;
		case BUILTIN_ROUNDEVEN:
			compiler.linking.link_math = true;
			llvm_emit_simple_builtin(c, result_value, expr, intrinsic_id.roundeven);
			return;
		case BUILTIN_SET_ROUNDING_MODE:
			{
				compiler.linking.link_math = true;
				Expr **args = expr->call_expr.arguments;
				LLVMValueRef arg_slots[1];
				llvm_emit_intrinsic_args(c, args, arg_slots, 1);
				llvm_value_set(result_value, llvm_emit_call_intrinsic(c, intrinsic_id.set_rounding, NULL, 0, arg_slots, 1), type_void);
			}
			return;
		case BUILTIN_WASM_MEMORY_GROW:
			// -1 on non-wasm
			if (!arch_is_wasm(compiler.platform.arch))
			{
				llvm_value_set(result_value, llvm_const_int(c, expr->type, -1), expr->type);
				return;
			}
			llvm_emit_builtin_args_types3(c, result_value, expr, intrinsic_id.wasm_memory_grow, expr->type, NULL, NULL);
			return;
		case BUILTIN_WASM_MEMORY_SIZE:
			if (!arch_is_wasm(compiler.platform.arch))
			{
				// 0 (no mem) on non-wasm.
				llvm_value_set(result_value, llvm_const_int(c, expr->type, 0), expr->type);
				return;
			}
			llvm_emit_builtin_args_types3(c, result_value, expr, intrinsic_id.wasm_memory_size, expr->type, NULL, NULL);
			return;
		case BUILTIN_SIN:
			compiler.linking.link_math = true;
			llvm_emit_simple_builtin(c, result_value, expr, intrinsic_id.sin);
			return;
		case BUILTIN_SQRT:
			compiler.linking.link_math = true;
			llvm_emit_simple_builtin(c, result_value, expr, intrinsic_id.sqrt);
			return;
		case BUILTIN_TRUNC:
			compiler.linking.link_math = true;
			llvm_emit_simple_builtin(c, result_value, expr, intrinsic_id.trunc);
			return;
		case BUILTIN_LRINT:
			compiler.linking.link_math = true;
			llvm_emit_simple_builtin(c, result_value, expr, intrinsic_id.lrint);
			return;
		case BUILTIN_LROUND:
			compiler.linking.link_math = true;
			llvm_emit_simple_builtin(c, result_value, expr, intrinsic_id.lround);
			return;
		case BUILTIN_LLRINT:
			compiler.linking.link_math = true;
			TODO
		case BUILTIN_LLROUND:
			compiler.linking.link_math = true;
			TODO
		case BUILTIN_STR_HASH:
		case BUILTIN_STR_LOWER:
		case BUILTIN_STR_UPPER:
		case BUILTIN_STR_FIND:
		case BUILTIN_STR_REPLACE:
		case BUILTIN_STR_SNAKECASE:
		case BUILTIN_STR_PASCALCASE:
		case BUILTIN_WIDESTRING_16:
		case BUILTIN_WIDESTRING_32:
		case BUILTIN_RND:
		case BUILTIN_SPRINTF:
			UNREACHABLE_VOID
		case BUILTIN_NONE:
			UNREACHABLE_VOID
	}
	UNREACHABLE_VOID
}
