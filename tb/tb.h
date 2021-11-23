//  _______ _             ____             _                  _
// |__   __(_)           |  _ \           | |                | |
//    | |   _ _ __  _   _| |_) | __ _  ___| | _____ _ __   __| |
//    | |  | | '_ \| | | |  _ < / _` |/ __| |/ / _ \ '_ \ / _` |
//    | |  | | | | | |_| | |_) | (_| | (__|   <  __/ | | | (_| |
//    |_|  |_|_| |_|\__, |____/ \__,_|\___|_|\_\___|_| |_|\__,_|
//                   __/ |
//                  |___/
//
//    It's a relatively small compiler backend all behind a
//    simple C API! To get started: TODO
//
#ifndef _TINYBACKEND_H_
#define _TINYBACKEND_H_

#ifdef __cplusplus
extern "C" {
#endif

	// Windows likes it's secure functions, i kinda do too
	// but only sometimes and this isn't one of them
#ifndef _CRT_SECURE_NO_WARNINGS
#define _CRT_SECURE_NO_WARNINGS
#endif

#include <stdio.h>
#include <assert.h>
#include <stdint.h>
#include <stdlib.h>
#include <time.h>
#include <string.h>
#include <stdbool.h>
#include <stdatomic.h>
#include <inttypes.h>

#ifndef TB_MAX_THREADS
#define TB_MAX_THREADS 16
#endif

// Per-thread
#ifndef TB_TEMPORARY_STORAGE_SIZE
#define TB_TEMPORARY_STORAGE_SIZE (1 << 20)
#endif

#ifndef TB_MAX_FUNCTIONS
#define TB_MAX_FUNCTIONS (1 << 22)
#endif

#define TB_HOST_UNKNOWN 0
#define TB_HOST_X86_64 1

// While generating the IR, it's possible to
// perform some optimizations on the spot such
// as CSE and constant folding, if this define
// is 0, the CSE is disabled.
#define TB_FRONTEND_OPT 0

// If on, the labels aren't marked in the object file
// might save on performance at the cost of some assembly
// readability.
#define TB_STRIP_LABELS 0

#if defined(__x86_64__) || defined(_M_X64) || defined(_M_AMD64)
#define TB_HOST_ARCH TB_HOST_X86_64
#else
#define TB_HOST_ARCH TB_HOST_UNKNOWN
#endif

#define TB_API extern

#define TB_NULL_REG ((TB_Register)0)
#define TB_REG_MAX ((TB_Register)INT32_MAX)

typedef enum TB_ArithmaticBehavior {
	// No overflow will assume the value does not
	// overflow and if it does this can be considered
	// undefined behavior with unknown consequences.
	// no signed wrap
	TB_ASSUME_NSW,
	// no unsigned wrap
	TB_ASSUME_NUW,

	// Wrapping will allow the integer to safely wrap.
	TB_CAN_WRAP,

	// Overflow check will throw an error if the result
	// cannot be represented in the resulting type.
	TB_SIGNED_TRAP_ON_WRAP,
	TB_UNSIGNED_TRAP_ON_WRAP,

	// Saturated arithmatic will clamp the results in the
	// event of overflow/underflow.
	TB_SATURATED_UNSIGNED,
	TB_SATURATED_SIGNED
} TB_ArithmaticBehavior;

typedef enum TB_Arch {
	TB_ARCH_X86_64,
	TB_ARCH_AARCH64
} TB_Arch;

typedef enum TB_System {
	TB_SYSTEM_WINDOWS,
	TB_SYSTEM_LINUX,

	// TODO(NeGate): Actually implement these lol
	TB_SYSTEM_MACOS,
	TB_SYSTEM_ANDROID
} TB_System;

typedef enum TB_BranchHint {
	TB_BRANCH_HINT_NONE,
	TB_BRANCH_HINT_LIKELY,
	TB_BRANCH_HINT_UNLIKELY
} TB_BranchHint;

typedef enum TB_OptLevel {
	TB_OPT_O0,

	// DCE
	// CSE
	// MEM2REG
	TB_OPT_O1,

	// DCE
	// CSE
	// MEM2REG
	TB_OPT_SIZE,

	// DCE
	// CSE
	// MEM2REG
	// LOOP_UNROLL
	TB_OPT_SPEED,
	} TB_OptLevel;

enum {
	TB_VOID,
	// Boolean
	TB_BOOL,
	// Integers
	TB_I8, TB_I16, TB_I32, TB_I64, TB_I128,
	// IEEE 754 Floating point
	TB_F32, TB_F64,
	// Pointers
	// NOTE(NeGate): consider support for multiple address spaces
	TB_PTR,

	TB_MAX_TYPES
};

#define TB_IS_INTEGER_TYPE(x) ((x) >= TB_I8 && (x) <= TB_I128)
#define TB_IS_FLOAT_TYPE(x) ((x) >= TB_F32 && (x) <= TB_F64)
#define TB_IS_POINTER_TYPE(x) ((x) == TB_PTR)

typedef struct TB_DataType {
	uint8_t type;
	uint8_t count; // 0 is illegal, except on VOID, it doesn't matter there
} TB_DataType;

typedef struct TB_Int128 {
	uint64_t lo;
	uint64_t hi;
} TB_Int128;

typedef struct TB_FeatureConstraints {
	int max_vector_width[TB_MAX_TYPES];
} TB_FeatureConstraints;

typedef struct TB_FeatureSet {
	struct {
		bool sse3 : 1;

		bool popcnt : 1;
		bool lzcnt : 1;
		bool sse41 : 1;
		bool sse42 : 1;

		bool clmul : 1;
		bool f16c : 1;

		bool bmi1 : 1;
		bool bmi2 : 1;

		bool avx : 1;
		bool avx2 : 1;
	} x64;
	struct {
		bool bf16 : 1;
	} aarch64;
} TB_FeatureSet;

typedef int TB_Label;

typedef struct TB_SwitchEntry {
	uint32_t key;
	TB_Label value;
} TB_SwitchEntry;

typedef int TB_Register;
typedef int TB_Reg; // short-hand

typedef int TB_FileID;
typedef int TB_ExternalID;
typedef struct TB_Module TB_Module;
typedef struct TB_Function TB_Function;
typedef struct TB_FunctionOutput TB_FunctionOutput;

// *******************************
// Public macros
// *******************************
#define TB_TYPE_VOID() (TB_DataType){ .type = TB_VOID }

#define TB_TYPE_I8(c) (TB_DataType){ .type = TB_I8, .count = c }
#define TB_TYPE_I16(c) (TB_DataType){ .type = TB_I16, .count = c }
#define TB_TYPE_I32(c) (TB_DataType){ .type = TB_I32, .count = c }
#define TB_TYPE_I64(c) (TB_DataType){ .type = TB_I64, .count = c }
#define TB_TYPE_I128(c) (TB_DataType){ .type = TB_I128, .count = c }

#define TB_TYPE_F32(c) (TB_DataType){ .type = TB_F32, .count = c }
#define TB_TYPE_F64(c) (TB_DataType){ .type = TB_F64, .count = c }

#define TB_TYPE_BOOL(c) (TB_DataType){ .type = TB_BOOL, .count = c }
#define TB_TYPE_PTR() (TB_DataType){ .type = TB_PTR, .count = 1 }

// *******************************
// Public functions
// *******************************
TB_API void tb_get_constraints(TB_Arch target_arch, const TB_FeatureSet* features, TB_FeatureConstraints* constraints);

TB_API TB_Module* tb_module_create(TB_Arch target_arch, TB_System target_system, const TB_FeatureSet* features, int optimization_level, int max_threads, bool preserve_ir_after_submit);
// preserve_ir_after_submit means that after the tb_module_compile_func(...) you can
// still access the IR, this comes at a higher overall memory usage cost since the
// IR is kept in memory for the lifetime of the compile but this is not an issue when
// debugging.

TB_API bool tb_module_compile_func(TB_Module* m, TB_Function* f);
TB_API size_t tb_DEBUG_module_get_full_node_count(TB_Module* m);
TB_API void tb_module_destroy(TB_Module* m);

TB_API bool tb_module_compile(TB_Module* m);
TB_API bool tb_module_export(TB_Module* m, FILE* f);
TB_API void tb_module_export_jit(TB_Module* m);

TB_API void* tb_module_get_jit_func_by_name(TB_Module* m, const char* name);
TB_API void* tb_module_get_jit_func_by_id(TB_Module* m, size_t i);
TB_API void* tb_module_get_jit_func(TB_Module* m, TB_Function* f);

TB_API TB_Function* tb_function_create(TB_Module* m, const char* name, TB_DataType return_dt);
TB_API TB_ExternalID tb_module_extern(TB_Module* m, const char* name);
TB_API TB_FileID tb_register_file(TB_Module* m, const char* path);

TB_API TB_Label tb_get_current_label(TB_Function* f);
TB_API void tb_inst_loc(TB_Function* f, TB_FileID file, int line);

TB_API TB_Register tb_inst_param(TB_Function* f, TB_DataType dt);
TB_API TB_Register tb_inst_param_addr(TB_Function* f, TB_Register param);

TB_API TB_Register tb_inst_sxt(TB_Function* f, TB_Register src, TB_DataType dt);
TB_API TB_Register tb_inst_zxt(TB_Function* f, TB_Register src, TB_DataType dt);

TB_API TB_Register tb_inst_local(TB_Function* f, uint32_t size, uint32_t alignment);
TB_API TB_Register tb_inst_load(TB_Function* f, TB_DataType dt, TB_Register addr, uint32_t alignment);
TB_API void tb_inst_store(TB_Function* f, TB_DataType dt, TB_Register addr, TB_Register val, uint32_t alignment);

TB_API TB_Register tb_inst_iconst(TB_Function* f, TB_DataType dt, uint64_t imm);
TB_API TB_Register tb_inst_iconst128(TB_Function* f, TB_DataType dt, TB_Int128 imm);

TB_API TB_Register tb_inst_fconst(TB_Function* f, TB_DataType dt, double imm);

TB_API TB_Register tb_inst_array_access(TB_Function* f, TB_Register base, TB_Register index, uint32_t stride);
TB_API TB_Register tb_inst_member_access(TB_Function* f, TB_Register base, int32_t offset);
TB_API TB_Register tb_inst_call(TB_Function* f, TB_DataType dt, const TB_Function* target, size_t param_count, const TB_Register* params);
TB_API TB_Register tb_inst_ecall(TB_Function* f, TB_DataType dt, const TB_ExternalID target, size_t param_count, const TB_Register* params);

TB_API void tb_inst_memset(TB_Function* f, TB_Register dst, TB_Register val, TB_Register size, int align);
TB_API void tb_inst_memcpy(TB_Function* f, TB_Register dst, TB_Register src, TB_Register size, int align);

TB_API TB_Register tb_inst_not(TB_Function* f, TB_DataType dt, TB_Register n);
TB_API TB_Register tb_inst_neg(TB_Function* f, TB_DataType dt, TB_Register n);

TB_API TB_Register tb_inst_and(TB_Function* f, TB_DataType dt, TB_Register a, TB_Register b);
TB_API TB_Register tb_inst_or(TB_Function* f, TB_DataType dt, TB_Register a, TB_Register b);
TB_API TB_Register tb_inst_xor(TB_Function* f, TB_DataType dt, TB_Register a, TB_Register b);
TB_API TB_Register tb_inst_add(TB_Function* f, TB_DataType dt, TB_Register a, TB_Register b, TB_ArithmaticBehavior arith_behavior);
TB_API TB_Register tb_inst_sub(TB_Function* f, TB_DataType dt, TB_Register a, TB_Register b, TB_ArithmaticBehavior arith_behavior);
TB_API TB_Register tb_inst_mul(TB_Function* f, TB_DataType dt, TB_Register a, TB_Register b, TB_ArithmaticBehavior arith_behavior);
TB_API TB_Register tb_inst_div(TB_Function* f, TB_DataType dt, TB_Register a, TB_Register b, bool signedness);

TB_API TB_Register tb_inst_shl(TB_Function* f, TB_DataType dt, TB_Register a, TB_Register b, TB_ArithmaticBehavior arith_behavior);
TB_API TB_Register tb_inst_sar(TB_Function* f, TB_DataType dt, TB_Register a, TB_Register b);
TB_API TB_Register tb_inst_shr(TB_Function* f, TB_DataType dt, TB_Register a, TB_Register b);

TB_API TB_Register tb_inst_fadd(TB_Function* f, TB_DataType dt, TB_Register a, TB_Register b);
TB_API TB_Register tb_inst_fsub(TB_Function* f, TB_DataType dt, TB_Register a, TB_Register b);
TB_API TB_Register tb_inst_fmul(TB_Function* f, TB_DataType dt, TB_Register a, TB_Register b);
TB_API TB_Register tb_inst_fdiv(TB_Function* f, TB_DataType dt, TB_Register a, TB_Register b);

TB_API TB_Register tb_inst_cmp_eq(TB_Function* f, TB_DataType dt, TB_Register a, TB_Register b);
TB_API TB_Register tb_inst_cmp_ne(TB_Function* f, TB_DataType dt, TB_Register a, TB_Register b);

TB_API TB_Register tb_inst_cmp_slt(TB_Function* f, TB_DataType dt, TB_Register a, TB_Register b);
TB_API TB_Register tb_inst_cmp_sle(TB_Function* f, TB_DataType dt, TB_Register a, TB_Register b);
TB_API TB_Register tb_inst_cmp_sgt(TB_Function* f, TB_DataType dt, TB_Register a, TB_Register b);
TB_API TB_Register tb_inst_cmp_sge(TB_Function* f, TB_DataType dt, TB_Register a, TB_Register b);

TB_API TB_Register tb_inst_cmp_ult(TB_Function* f, TB_DataType dt, TB_Register a, TB_Register b);
TB_API TB_Register tb_inst_cmp_ule(TB_Function* f, TB_DataType dt, TB_Register a, TB_Register b);
TB_API TB_Register tb_inst_cmp_ugt(TB_Function* f, TB_DataType dt, TB_Register a, TB_Register b);
TB_API TB_Register tb_inst_cmp_uge(TB_Function* f, TB_DataType dt, TB_Register a, TB_Register b);

TB_API TB_Register tb_inst_cmp_flt(TB_Function* f, TB_DataType dt, TB_Register a, TB_Register b);
TB_API TB_Register tb_inst_cmp_fle(TB_Function* f, TB_DataType dt, TB_Register a, TB_Register b);
TB_API TB_Register tb_inst_cmp_fgt(TB_Function* f, TB_DataType dt, TB_Register a, TB_Register b);
TB_API TB_Register tb_inst_cmp_fge(TB_Function* f, TB_DataType dt, TB_Register a, TB_Register b);

TB_API TB_Label tb_inst_new_label_id(TB_Function* f);
// Gives you a reference to a local label, doesn't place it anywhere.

TB_API TB_Register tb_inst_phi2(TB_Function* f, TB_DataType dt, TB_Label a_label, TB_Register a, TB_Label b_label, TB_Register b);
TB_API TB_Register tb_inst_label(TB_Function* f, TB_Label id);
TB_API void tb_inst_goto(TB_Function* f, TB_Label id);
TB_API TB_Register tb_inst_if(TB_Function* f, TB_Register cond, TB_Label if_true, TB_Label if_false);
TB_API void tb_inst_switch(TB_Function* f, TB_DataType dt, TB_Register key, TB_Label default_label, size_t entry_count, const TB_SwitchEntry* entries);
TB_API void tb_inst_ret(TB_Function* f, TB_DataType dt, TB_Register value);

TB_API void tb_function_print(TB_Function* f, FILE* out);

#ifdef __cplusplus
}
#endif
#endif /* _TINYBACKEND_H_ */
