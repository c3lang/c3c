#pragma once
#include <llvm-c/TargetMachine.h>

#ifdef __cplusplus
extern "C" {
#endif

extern int llvm_version_major;

typedef enum
{
	DEFAULT = -1,
	STRICT,
	RELAXED,
	FAST
} FastMathOption;

typedef enum
{
	AR_GNU,
	AR_DARWIN,
	AR_DARWIN64,
	AR_BSD,
	AR_GNU64,
	AR_COFF,
} ArFormat;

typedef enum
{
	LLVM_O0,
	LLVM_O1,
	LLVM_O2,
	LLVM_O3,
	LLVM_Os,
	LLVM_Oz
} LLVMOptLevels;

typedef struct
{
	bool should_debug;
	bool should_verify;
	LLVMOptLevels opt_level;
	bool is_kernel;
	struct
	{
		bool recover;
		bool mem_sanitize;
		bool mem_track_origins;
		bool mem_retval;
		bool thread_sanitize;
		bool address_sanitize;
		bool asan_use_globals_gc;
		bool asan_use_odr_indicator;
		bool asan_use_after_scope;
		bool asan_use_after_return;
		bool asan_use_global_dstor;
		bool hwaddress_sanitize;
	} sanitizer;
	struct
	{
		bool unroll_loops;
		bool interleave_loops;
		bool vectorize_loops;
		bool slp_vectorize;
		bool merge_functions;
	} opt;
} LLVMPasses;

bool llvm_run_passes(LLVMModuleRef m, LLVMTargetMachineRef tm, LLVMPasses *passes);
bool llvm_link_elf(const char **args, int arg_count, const char **error_string);
bool llvm_link_macho(const char **args, int arg_count, const char **error_string);
bool llvm_link_coff(const char **args, int arg_count, const char **error_string);
bool llvm_link_wasm(const char **args, int arg_count, const char **error_string);
bool llvm_link_mingw(const char **args, int arg_count, const char **error_string);
bool llvm_ar(const char *out_name, const char **args, size_t count, int ArFormat);

LLVMValueRef LLVMConstBswap(LLVMValueRef ConstantVal);
void LLVMBuilderSetFastMathFlags(LLVMBuilderRef Builder, FastMathOption option);
void LLVMSetDSOLocal(LLVMValueRef Global, bool value);
void LLVMSetTargetMachineUseInitArray(LLVMTargetMachineRef ref, bool use_init_array);

#ifdef __cplusplus
}
#endif