//
//  _______ _ _     _      ____  ______
// |__   __(_) |   | |    |  _ \|  ____|
//    | |   _| | __| | ___| |_) | |__
//    | |  | | |/ _` |/ _ \  _ <|  __|
//    | |  | | | (_| |  __/ |_) | |____
//    |_|  |_|_|\__,_|\___|____/|______|
//
//    It's a relatively small compiler backend all
//    behind a simple C API!
//
#ifndef _TILDEBACKEND_H_
#define _TILDEBACKEND_H_

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
#include <string.h>
#include <stdbool.h>
#include <inttypes.h>

// https://semver.org/
#define TB_VERSION_MAJOR 0
#define TB_VERSION_MINOR 1
#define TB_VERSION_PATCH 0

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

#define TB_API extern

#define TB_HOST_UNKNOWN 0
#define TB_HOST_X86_64 1

// If on, the labels aren't marked in the object file
// might save on performance at the cost of some assembly
// readability.
#define TB_STRIP_LABELS 1

#if defined(__x86_64__) || defined(_M_X64) || defined(_M_AMD64)
#define TB_HOST_ARCH TB_HOST_X86_64
#else
#define TB_HOST_ARCH TB_HOST_UNKNOWN
#endif

#define TB_NULL_REG ((TB_Register)0)
#define TB_REG_MAX ((TB_Register)INT_MAX)

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

typedef enum TB_CallingConv {
	TB_CDECL,
	TB_STDCALL
} TB_CallingConv;

typedef enum TB_BranchHint {
	TB_BRANCH_HINT_NONE,
	TB_BRANCH_HINT_LIKELY,
	TB_BRANCH_HINT_UNLIKELY
} TB_BranchHint;

typedef enum TB_OptLevel {
	// no optimizer run
	TB_OPT_O0,

	// run optimizer with all optimizations
	TB_OPT_O1,

	// same as O1 but favors size, will aggresively deduplicate
	// (at least that's the plan :P)
	TB_OPT_SIZE,

	// same as O1 but favors speed, will aggresively unroll
	// sometimes (at least that's the plan :P)
	TB_OPT_SPEED,
	} TB_OptLevel;

typedef enum TB_DataTypeEnum {
	TB_VOID,
	// Boolean
	TB_BOOL,
	// Integers
	TB_I8, TB_I16, TB_I32, TB_I64,
	// IEEE 754 Floating point
	TB_F32, TB_F64,
	// Pointers
	TB_PTR,

	TB_MAX_TYPES
} TB_DataTypeEnum;

#define TB_IS_INTEGER_TYPE(x) ((x) >= TB_I8 && (x) <= TB_I64)
#define TB_IS_FLOAT_TYPE(x) ((x) >= TB_F32 && (x) <= TB_F64)
#define TB_IS_POINTER_TYPE(x) ((x) == TB_PTR)

typedef struct TB_DataType {
	TB_DataTypeEnum type : 8;
	uint8_t count; // 0 is illegal, except on VOID, it doesn't matter there
} TB_DataType;

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

typedef unsigned int TB_FileID;
typedef unsigned int TB_FunctionID;
typedef unsigned int TB_ExternalID;
typedef unsigned int TB_GlobalID;
typedef unsigned int TB_InitializerID;

typedef struct TB_Module TB_Module;
typedef struct TB_Function TB_Function;
typedef struct TB_FunctionPrototype TB_FunctionPrototype;

// *******************************
// Public macros
// *******************************
#ifdef __cplusplus
#define TB_TYPE_VOID TB_DataType{ TB_VOID, 1 }

#define TB_TYPE_I8 TB_DataType{ TB_I8, 1 }
#define TB_TYPE_I16 TB_DataType{ TB_I16, 1 }
#define TB_TYPE_I32 TB_DataType{ TB_I32, 1 }
#define TB_TYPE_I64 TB_DataType{ TB_I64, 1 }

#define TB_TYPE_F32 TB_DataType{ TB_F32, 1 }
#define TB_TYPE_F64 TB_DataType{ TB_F64, 1 }

#define TB_TYPE_BOOL TB_DataType{ TB_BOOL, 1 }
#define TB_TYPE_PTR TB_DataType{ TB_PTR, 1 }

#define TB_TYPE_VEC_I8(c) TB_DataType{ TB_I8, c }
#define TB_TYPE_VEC_I16(c) TB_DataType{ TB_I16, c }
#define TB_TYPE_VEC_I32(c) TB_DataType{ TB_I32, c }
#define TB_TYPE_VEC_I64(c) TB_DataType{ TB_I64, c }
#define TB_TYPE_VEC_F32(c) TB_DataType{ TB_F32, c }
#define TB_TYPE_VEC_F64(c) TB_DataType{ TB_F64, c }
#define TB_TYPE_VEC_BOOL(c) TB_DataType{ TB_BOOL, c }

#else

#define TB_TYPE_VOID (TB_DataType){ TB_VOID, 1 }

#define TB_TYPE_I8 (TB_DataType){ TB_I8, 1 }
#define TB_TYPE_I16 (TB_DataType){ TB_I16, 1 }
#define TB_TYPE_I32 (TB_DataType){ TB_I32, 1 }
#define TB_TYPE_I64 (TB_DataType){ TB_I64, 1 }

#define TB_TYPE_F32 (TB_DataType){ TB_F32, 1 }
#define TB_TYPE_F64 (TB_DataType){ TB_F64, 1 }

#define TB_TYPE_BOOL (TB_DataType){ TB_BOOL, 1 }
#define TB_TYPE_PTR (TB_DataType){ TB_PTR, 1 }

#define TB_TYPE_VEC_I8(c) (TB_DataType){ TB_I8, c }
#define TB_TYPE_VEC_I16(c) (TB_DataType){ TB_I16, c }
#define TB_TYPE_VEC_I32(c) (TB_DataType){ TB_I32, c }
#define TB_TYPE_VEC_I64(c) (TB_DataType){ TB_I64, c }
#define TB_TYPE_VEC_F32(c) (TB_DataType){ TB_F32, c }
#define TB_TYPE_VEC_F64(c) (TB_DataType){ TB_F64, c }

#endif

////////////////////////////////
// Module management
////////////////////////////////
// Creates a module with the correct target and settings
TB_API TB_Module* tb_module_create(TB_Arch target_arch, TB_System target_system, const TB_FeatureSet* features);

// Validates IR & compiles the function into machine code.
// returns false if it fails.
TB_API bool tb_module_compile_func(TB_Module* m, TB_Function* f);

// NOTE: Don't use this it's for testing purposes.
TB_API size_t tb_DEBUG_module_get_full_node_count(TB_Module* m);

// Frees all resources for the TB_Module and it's functions, globals and compiled code.
TB_API void tb_module_destroy(TB_Module* m);

// Waits for the machine code generation to finish before continuing.
TB_API bool tb_module_compile(TB_Module* m);

// Exports an object file with all the machine code and symbols generated.
TB_API bool tb_module_export(TB_Module* m, const char* path);

TB_API void tb_module_export_jit(TB_Module* m);

TB_API void* tb_module_get_jit_func_by_name(TB_Module* m, const char* name);
TB_API void* tb_module_get_jit_func_by_id(TB_Module* m, size_t i);
TB_API void* tb_module_get_jit_func(TB_Module* m, TB_Function* f);

// Binds an external to an address
TB_API bool tb_jit_import(TB_Module* m, const char* name, void* address);

TB_API TB_ExternalID tb_extern_create(TB_Module* m, const char* name);
TB_API TB_FileID tb_file_create(TB_Module* m, const char* path);

////////////////////////////////
// Function Prototypes
////////////////////////////////
// creates a function prototype used to define a function's parameters and
// return type.
//
// function prototypes do not get freed individually and last for the entire run
// of the backend, they can also be reused for multiple functions which have matching
// signatures.
TB_API TB_FunctionPrototype* tb_prototype_create(TB_Module* m, TB_CallingConv conv, TB_DataType return_dt, int num_params, bool has_varargs);

// adds a parameter to the function prototype, TB doesn't support struct
// parameters so the frontend must lower them to pointers or any other type
// depending on their preferred ABI.
TB_API void tb_prototype_add_param(TB_FunctionPrototype* p, TB_DataType dt);

// same as tb_prototype_add_param(...) but takes an array
TB_API void tb_prototype_add_params(TB_FunctionPrototype* p, size_t count, const TB_DataType* dt);

// adds a parameter to the function prototype, TB doesn't support struct
// parameters so the frontend must lower them to pointers or any other type
// depending on their preferred ABI.
TB_API TB_Function* tb_prototype_build(TB_Module* m, TB_FunctionPrototype* p, const char* name);

////////////////////////////////
// Constant Initializers
////////////////////////////////
// NOTE: the max objects is a cap and thus it can be bigger than the actual
// number used.
TB_API TB_InitializerID tb_initializer_create(TB_Module* m, size_t size, size_t align, size_t max_objects);

// returns a buffer which the user can fill to then have represented in the initializer
TB_API void* tb_initializer_add_region(TB_Module* m, TB_InitializerID id, size_t offset, size_t size);

////////////////////////////////
// Constant Initializers
////////////////////////////////
TB_API TB_GlobalID tb_global_create(TB_Module* m, const char* name, TB_InitializerID initializer);

////////////////////////////////
// Function IR Generation
////////////////////////////////
TB_API TB_Function* tb_function_clone(TB_Module* m, TB_Function* f, const char* name);
TB_API void tb_function_print(TB_Function* f, FILE* out);
TB_API void tb_function_free(TB_Function* f);

TB_API TB_Label tb_inst_get_current_label(TB_Function* f);
TB_API void tb_inst_loc(TB_Function* f, TB_FileID file, int line);

TB_API TB_Register tb_inst_param(TB_Function* f, int param_id);
TB_API TB_Register tb_inst_param_addr(TB_Function* f, int param_id);

TB_API TB_Register tb_inst_fpxt(TB_Function* f, TB_Register src, TB_DataType dt);
TB_API TB_Register tb_inst_sxt(TB_Function* f, TB_Register src, TB_DataType dt);
TB_API TB_Register tb_inst_zxt(TB_Function* f, TB_Register src, TB_DataType dt);
TB_API TB_Register tb_inst_trunc(TB_Function* f, TB_Register src, TB_DataType dt);
TB_API TB_Register tb_inst_int2ptr(TB_Function* f, TB_Register src);
TB_API TB_Register tb_inst_ptr2int(TB_Function* f, TB_Register src, TB_DataType dt);

TB_API TB_Register tb_inst_local(TB_Function* f, uint32_t size, uint32_t alignment);
TB_API TB_Register tb_inst_load(TB_Function* f, TB_DataType dt, TB_Register addr, uint32_t alignment);
TB_API void tb_inst_store(TB_Function* f, TB_DataType dt, TB_Register addr, TB_Register val, uint32_t alignment);

TB_API TB_Register tb_inst_volatile_load(TB_Function* f, TB_DataType dt, TB_Register addr, uint32_t alignment);
TB_API void tb_inst_volatile_store(TB_Function* f, TB_DataType dt, TB_Register addr, TB_Register val, uint32_t alignment);

TB_API void tb_inst_initialize_mem(TB_Function* f, TB_Register addr, TB_InitializerID src);

TB_API TB_Register tb_inst_iconst(TB_Function* f, TB_DataType dt, uint64_t imm);
TB_API TB_Register tb_inst_fconst(TB_Function* f, TB_DataType dt, double imm);

// string is a UTF-8 null terminated string
TB_API TB_Register tb_inst_const_cstr(TB_Function* f, const char* str);

// string is a slice of bytes
TB_API TB_Register tb_inst_const_string(TB_Function* f, const char* str, size_t len);

TB_API TB_Register tb_inst_array_access(TB_Function* f, TB_Register base, TB_Register index, uint32_t stride);
TB_API TB_Register tb_inst_member_access(TB_Function* f, TB_Register base, int32_t offset);

TB_API TB_Register tb_inst_get_func_address(TB_Function* f, const TB_Function* target);
TB_API TB_Register tb_inst_get_extern_address(TB_Function* f, TB_ExternalID target);
TB_API TB_Register tb_inst_get_global_address(TB_Function* f, TB_GlobalID target);

TB_API TB_Register tb_inst_call(TB_Function* f, TB_DataType dt, const TB_Function* target, size_t param_count, const TB_Register* params);
TB_API TB_Register tb_inst_vcall(TB_Function* f, TB_DataType dt, TB_Register target, size_t param_count, const TB_Register* params);
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

TB_API TB_Register tb_inst_cmp_ilt(TB_Function* f, TB_DataType dt, TB_Register a, TB_Register b, bool signedness);
TB_API TB_Register tb_inst_cmp_ile(TB_Function* f, TB_DataType dt, TB_Register a, TB_Register b, bool signedness);
TB_API TB_Register tb_inst_cmp_igt(TB_Function* f, TB_DataType dt, TB_Register a, TB_Register b, bool signedness);
TB_API TB_Register tb_inst_cmp_ige(TB_Function* f, TB_DataType dt, TB_Register a, TB_Register b, bool signedness);

TB_API TB_Register tb_inst_cmp_flt(TB_Function* f, TB_DataType dt, TB_Register a, TB_Register b);
TB_API TB_Register tb_inst_cmp_fle(TB_Function* f, TB_DataType dt, TB_Register a, TB_Register b);
TB_API TB_Register tb_inst_cmp_fgt(TB_Function* f, TB_DataType dt, TB_Register a, TB_Register b);
TB_API TB_Register tb_inst_cmp_fge(TB_Function* f, TB_DataType dt, TB_Register a, TB_Register b);

// Gives you a reference to a local label, doesn't place it anywhere.
TB_API TB_Label tb_inst_new_label_id(TB_Function* f);

TB_API TB_Register tb_inst_phi2(TB_Function* f, TB_DataType dt, TB_Label a_label, TB_Register a, TB_Label b_label, TB_Register b);
TB_API TB_Register tb_inst_label(TB_Function* f, TB_Label id);
TB_API void tb_inst_goto(TB_Function* f, TB_Label id);
TB_API TB_Register tb_inst_if(TB_Function* f, TB_Register cond, TB_Label if_true, TB_Label if_false);
TB_API void tb_inst_switch(TB_Function* f, TB_DataType dt, TB_Register key, TB_Label default_label, size_t entry_count, const TB_SwitchEntry* entries);
TB_API void tb_inst_ret(TB_Function* f, TB_Register value);

////////////////////////////////
// Optimizer
////////////////////////////////
TB_API void tb_function_optimize(TB_Function* f, TB_OptLevel opt);

TB_API bool tb_opt_mem2reg(TB_Function* f);
TB_API bool tb_opt_dead_expr_elim(TB_Function* f);
TB_API bool tb_opt_dead_block_elim(TB_Function* f);
TB_API bool tb_opt_fold(TB_Function* f);
TB_API bool tb_opt_load_elim(TB_Function* f);
TB_API bool tb_opt_inline(TB_Function* f);
TB_API bool tb_opt_hoist_locals(TB_Function* f);
TB_API bool tb_opt_canonicalize(TB_Function* f);
TB_API bool tb_opt_deshort_circuit(TB_Function* f);
TB_API bool tb_opt_remove_pass_node(TB_Function* f);
TB_API bool tb_opt_strength_reduction(TB_Function* f);
TB_API bool tb_opt_compact_dead_regs(TB_Function* f);
TB_API bool tb_opt_copy_elision(TB_Function* f);

////////////////////////////////
// IR access
////////////////////////////////
TB_API TB_FunctionID tb_function_get_id(TB_Module* m, TB_Function* f);
TB_API TB_Function* tb_get_function_by_id(TB_Module* m, TB_FunctionID id);

TB_API TB_Register tb_node_get_last_register(TB_Function* f);
TB_API TB_DataType tb_node_get_data_type(TB_Function* f, TB_Register r);

// Returns the size and alignment of a LOCAL node, both must
// be valid addresses
TB_API void tb_get_function_get_local_info(TB_Function* f, TB_Register r, int* size, int* align);

// is an IF node?
TB_API bool tb_node_is_conditional(TB_Function* f, TB_Register r);

// is an IF, GOTO, RET, SWITCH, or LABEL node?
TB_API bool tb_node_is_terminator(TB_Function* f, TB_Register r);

TB_API bool tb_node_is_label(TB_Function* f, TB_Register r);

TB_API TB_Register tb_node_store_get_address(TB_Function* f, TB_Register r);
TB_API TB_Register tb_node_store_get_value(TB_Function* f, TB_Register r);

TB_API TB_Register tb_node_load_get_address(TB_Function* f, TB_Register r);

// These work for any floating point, comparison, or integer arithmatic ops
TB_API TB_Register tb_node_arith_get_left(TB_Function* f, TB_Register r);
TB_API TB_Register tb_node_arith_get_right(TB_Function* f, TB_Register r);

#ifdef __cplusplus
}
#endif
#endif /* _TINYBACKEND_H_ */
