#pragma once
// Copyright (c) 2019-2023 Christoffer Lerno. All rights reserved.
// Use of this source code is governed by the GNU LGPLv3.0 license
// a copy of which can be found in the LICENSE file.

#include "../version.h"
#include "../utils/lib.h"

#define MAX_LIB_DIRS 1024
#define MAX_FILES 2048
#define MAX_INCLUDES 2048
#define MAX_THREADS 0xFFFF
#define DEFAULT_SYMTAB_SIZE (256 * 1024)
#define DEFAULT_SWITCHRANGE_MAX_SIZE (256)

typedef enum
{
	BACKEND_LLVM = 1,
	BACKEND_TB = 2
} CompilerBackend;

typedef enum
{
	COMMAND_MISSING = 0,
	COMMAND_COMPILE,
	COMMAND_COMPILE_ONLY,
	COMMAND_COMPILE_BENCHMARK,
	COMMAND_COMPILE_TEST,
	COMMAND_GENERATE_HEADERS,
	COMMAND_INIT,
	COMMAND_INIT_LIB,
	COMMAND_BUILD,
	COMMAND_COMPILE_RUN,
	COMMAND_STATIC_LIB,
	COMMAND_DYNAMIC_LIB,
	COMMAND_RUN,
	COMMAND_CLEAN_RUN,
	COMMAND_CLEAN,
	COMMAND_VENDOR_FETCH,
	COMMAND_DIST,
	COMMAND_DOCS,
	COMMAND_BENCH,
	COMMAND_BENCHMARK,
	COMMAND_TEST,
	COMMAND_UNIT_TEST,
	COMMAND_PRINT_SYNTAX,
} CompilerCommand;

typedef enum
{
	DIAG_NONE = 0, // Don't use!
	DIAG_WARNING_TYPE, // Don't use!
	DIAG_UNUSED,
	DIAG_UNUSED_PARAMETER,
	DIAG_UNUSED_FUNCTION,
	DIAG_UNUSED_VARIABLE,
	DIAG_UNUSED_IMPORT,
	DIAG_UNUSED_MODULE,
	DIAG_UNUSED_LABEL,
	DIAG_UNUSED_PUBLIC,
	DIAG_UNUSED_TYPE,
	DIAG_CONVERSION,
	DIAG_COVERED_SWITCH_DEFAULT,
	DIAG_ERROR_TYPE, // Don't use this!
	DIAG_DUPLICATE_ATTRIBUTE,
	DIAG_NOT_IN_ENUM,
	DIAG_MISSING_CASE,
	DIAG_INT_TO_POINTER_CAST,
	DIAG_END_SENTINEL
} DiagnosticsType;

typedef enum
{
	DIAG_IGNORE = 0,
	DIAG_WARN,
	DIAG_ERROR,
} DiagnosticsSeverity;

typedef enum
{
	SAFETY_NOT_SET = -1,
	SAFETY_OFF = 0,
	SAFETY_ON = 1,
} SafetyLevel;

typedef enum
{
	TRUST_NONE,
	TRUST_INCLUDE,
	TRUST_FULL
} TrustLevel;

typedef enum
{
	COMPILE_NORMAL,
	COMPILE_LEX_ONLY,
	COMPILE_LEX_PARSE_ONLY,
	COMPILE_LEX_PARSE_CHECK_ONLY,
	COMPILE_OUTPUT_HEADERS,
	COMPILE_OUTPUT_AST,
} CompileOption;

typedef enum
{
	OPT_SETTING_NOT_SET = -1,
	OPT_SETTING_O0 = 0,
	OPT_SETTING_O1,
	OPT_SETTING_O2,
	OPT_SETTING_O3,
	OPT_SETTING_O4,
	OPT_SETTING_O5,
	OPT_SETTING_OSMALL,
	OPT_SETTING_OTINY,
} OptimizationSetting;

typedef enum
{
	OPTIMIZATION_NOT_SET = -1,
	OPTIMIZATION_NONE = 0,          // -O0
	OPTIMIZATION_LESS = 1,          // -O1
	OPTIMIZATION_MORE = 2,          // -O2
	OPTIMIZATION_AGGRESSIVE = 3,    // -O3
} OptimizationLevel;

typedef enum
{
	SINGLE_MODULE_NOT_SET = -1,
	SINGLE_MODULE_OFF = 0,
	SINGLE_MODULE_ON = 1
} SingleModule;

typedef enum
{
	SYSTEM_LINKER_NOT_SET = -1,
	SYSTEM_LINKER_OFF = 0,
	SYSTEM_LINKER_ON = 1
} SystemLinker;

typedef enum
{
	STRIP_UNUSED_NOT_SET = -1,
	STRIP_UNUSED_OFF = 0,
	STRIP_UNUSED_ON = 1
} StripUnused;

typedef enum
{
	LINK_LIBC_NOT_SET = -1,
	LINK_LIBC_OFF = 0,
	LINK_LIBC_ON = 1
} LinkLibc;

typedef enum
{
	EMIT_STDLIB_NOT_SET = -1,
	EMIT_STDLIB_OFF = 0,
	EMIT_STDLIB_ON = 1
} EmitStdlib;

typedef enum
{
	USE_STDLIB_NOT_SET = -1,
	USE_STDLIB_OFF = 0,
	USE_STDLIB_ON = 1
} UseStdlib;

typedef enum
{
	SIZE_OPTIMIZATION_NOT_SET = -1,
	SIZE_OPTIMIZATION_NONE = 0,     // None
	SIZE_OPTIMIZATION_SMALL = 1,    // -Os
	SIZE_OPTIMIZATION_TINY = 2,     // -Oz
} SizeOptimizationLevel;

typedef enum
{
	SOFT_FLOAT_DEFAULT = -1,
	SOFT_FLOAT_NONE = 0,
	SOFT_FLOAT_YES = 1
} SoftFloat;

typedef enum
{
	STRUCT_RETURN_DEFAULT = -1,
	STRUCT_RETURN_STACK = 0,
	STRUCT_RETURN_REG = 1
} StructReturn;

typedef enum
{
	X86VECTOR_DEFAULT = -1,
	X86VECTOR_NONE = 0,
	X86VECTOR_MMX = 1,
	X86VECTOR_SSE = 2,
	X86VECTOR_AVX = 3,
	X86VECTOR_AVX512 = 4,
	X86VECTOR_NATIVE = 5,
} X86VectorCapability;

typedef enum
{
	X86CPU_DEFAULT = -1,
	X86CPU_BASELINE = 0,
	X86CPU_SSSE3 = 1,
	X86CPU_SSE4 = 2,
	X86CPU_AVX1 = 3,
	X86CPU_AVX2_V1 = 4,
	X86CPU_AVX2_V2 = 5,
	X86CPU_AVX512 = 6,
	X86CPU_NATIVE = 7,
} X86CpuSet;

typedef enum
{
	FP_DEFAULT = -1,
	FP_STRICT = 0,
	FP_RELAXED,
	FP_FAST,
} FpOpt;

typedef enum
{
	RISCVFLOAT_DEFAULT = -1,
	RISCVFLOAT_NONE = 0,
	RISCVFLOAT_FLOAT = 1,
	RISCVFLOAT_DOUBLE = 2,
} RiscvFloatCapability;

typedef enum
{
	MEMORY_ENV_NOT_SET = -1,
	MEMORY_ENV_NORMAL = 0,
	MEMORY_ENV_SMALL = 1,
	MEMORY_ENV_TINY = 2,
	MEMORY_ENV_NONE = 3,
} MemoryEnvironment;

typedef enum
{
	WIN_CRT_DEFAULT = -1,
	WIN_CRT_NONE = 0,
	WIN_CRT_DYNAMIC = 1,
	WIN_CRT_STATIC = 2,
} WinCrtLinking;

typedef enum
{
	RELOC_DEFAULT = -1,
	RELOC_NONE = 0,
	RELOC_SMALL_PIC = 1,
	RELOC_BIG_PIC = 2,
	RELOC_SMALL_PIE = 3,
	RELOC_BIG_PIE = 4,
} RelocModel;

typedef enum
{
	DEBUG_INFO_NOT_SET = -1,
	DEBUG_INFO_NONE,
	DEBUG_INFO_LINE_TABLES,
	DEBUG_INFO_FULL
} DebugInfo;

typedef enum
{
	ARCH_OS_TARGET_DEFAULT = 0,
	ELF_AARCH64,
	ELF_RISCV32,
	ELF_RISCV64,
	ELF_X86,
	ELF_X64,
	FREEBSD_X86,
	FREEBSD_X64,
	LINUX_AARCH64,
	LINUX_RISCV32,
	LINUX_RISCV64,
	LINUX_X86,
	LINUX_X64,
	MACOS_AARCH64,
	MACOS_X64,
	MCU_X86,
	MINGW_X64,
	NETBSD_X86,
	NETBSD_X64,
	OPENBSD_X86,
	OPENBSD_X64,
	WASM32,
	WASM64,
	WINDOWS_AARCH64,
	WINDOWS_X64,
	ARCH_OS_TARGET_LAST = WINDOWS_X64
} ArchOsTarget;

#define ANY_WINDOWS_ARCH_OS WINDOWS_AARCH64: case WINDOWS_X64: case MINGW_X64

typedef struct BuildOptions_
{
	const char *lib_dir[MAX_LIB_DIRS];
	int lib_dir_count;
	const char *libs[MAX_LIB_DIRS];
	int lib_count;
	const char* linker_args[MAX_LIB_DIRS];
	int linker_arg_count;
	const char* linker_lib_dir[MAX_LIB_DIRS];
	int linker_lib_dir_count;
	const char* linker_libs[MAX_LIB_DIRS];
	int linker_lib_count;
	const char* std_lib_dir;
	struct {
		const char *sdk;
		const char *def;
		WinCrtLinking crt_linking;
	} win;
	struct {
		const char *sysroot;
		const char *min_version;
		const char *sdk_version;
	} macos;
	struct {
		const char *crt;
		const char *crtbegin;
	} linuxpaths;
	int build_threads;
	const char **libraries_to_fetch;
	const char **files;
	const char **feature_names;
	const char **removed_feature_names;
	const char *output_name;
	const char *project_name;
	const char *target_select;
	const char *path;
	const char *template;
	uint32_t symtab_size;
	unsigned version;
	CompilerBackend backend;
	CompilerCommand command;
	CompileOption compile_option;
	TrustLevel trust_level;
	DiagnosticsSeverity severity[DIAG_END_SENTINEL];
	OptimizationSetting optsetting;
	DebugInfo debug_info_override;
	ArchOsTarget arch_os_target_override;
	SafetyLevel safety_level;
	SingleModule single_module;
	bool emit_llvm;
	bool emit_asm;
	bool benchmark_mode;
	bool test_mode;
	bool no_entry;
	bool no_obj;
	bool read_stdin;
	bool print_output;
	const char *panicfn;
	const char *benchfn;
	const char *testfn;
	const char *cc;
	const char *build_dir;
	const char *llvm_out;
	const char *asm_out;
	const char *obj_out;
	const char *script_dir;
	RelocModel reloc_model;
	X86VectorCapability x86_vector_capability;
	X86CpuSet x86_cpu_set;
	FpOpt fp_math;
	EmitStdlib emit_stdlib;
	UseStdlib use_stdlib;
	LinkLibc link_libc;
	SystemLinker system_linker;
	StripUnused strip_unused;
	OptimizationLevel optlevel;
	SizeOptimizationLevel optsize;
	RiscvFloatCapability riscv_float_capability;
	MemoryEnvironment memory_environment;
	bool print_keywords;
	bool print_attributes;
	bool print_builtins;
	bool print_operators;
	bool print_type_properties;
	bool print_project_properties;
	bool print_precedence;
	bool print_build_settings;
	bool print_linking;
	bool benchmarking;
	bool testing;
} BuildOptions;

typedef enum
{
	TARGET_TYPE_EXECUTABLE,
	TARGET_TYPE_STATIC_LIB,
	TARGET_TYPE_DYNAMIC_LIB,
	TARGET_TYPE_OBJECT_FILES,
	TARGET_TYPE_BENCHMARK,
	TARGET_TYPE_TEST,
} TargetType;

typedef struct
{
	ArchOsTarget arch_os;
	const char **execs;
	const char **link_flags;
	const char **linked_libs;
	const char **depends;
} LibraryTarget;

typedef struct
{
	const char *dir;
	const char *provides;
	const char **depends;
	const char **execs;
	LibraryTarget *target_used;
	LibraryTarget **targets;
} Library;


typedef struct
{
	TargetType type;
	Library **library_list;
	const char *name;
	const char *version;
	const char *langrev;
	const char **source_dirs;
	const char **sources;
	const char **libdirs;
	const char **libs;
	const char **linker_libdirs;
	const char **linker_libs;
	const char *cpu;
	const char **link_args;
	const char *build_dir;
	const char *object_file_dir;
	const char *output_dir;
	const char *ir_file_dir;
	const char *asm_file_dir;
	const char *script_dir;
	bool run_after_compile;
	bool generate_benchmark_runner;
	bool generate_test_runner;
	bool benchmark_output;
	bool test_output;
	bool output_headers;
	bool output_ast;
	bool lex_only;
	bool parse_only;
	bool check_only;
	bool emit_llvm;
	bool emit_asm;
	bool emit_object_files;
	bool benchmarking;
	bool testing;
	bool read_stdin;
	bool print_output;
	bool print_linking;
	bool no_entry;
	int build_threads;
	TrustLevel trust_level;
	OptimizationSetting optsetting;
	OptimizationLevel optlevel;
	MemoryEnvironment memory_environment;
	SizeOptimizationLevel optsize;
	SingleModule single_module;
	UseStdlib use_stdlib;
	EmitStdlib emit_stdlib;
	LinkLibc link_libc;
	StripUnused strip_unused;
	DebugInfo debug_info;
	RelocModel reloc_model;
	ArchOsTarget arch_os_target;
	CompilerBackend backend;
	SystemLinker system_linker;
	uint32_t symtab_size;
	uint32_t switchrange_max_size;
	const char *panicfn;
	const char *benchfn;
	const char *testfn;
	const char *cc;
	const char *cflags;
	const char **exec;
	const char **csource_dirs;
	const char **csources;
	const char **feature_list;
	struct
	{
		SoftFloat soft_float : 3;
		StructReturn x86_struct_return : 3;
		X86VectorCapability x86_vector_capability : 4;
		RiscvFloatCapability riscv_float_capability : 4;
		bool trap_on_wrap : 1;
		FpOpt fp_math;
		SafetyLevel safe_mode;
		X86CpuSet x86_cpu_set;
	} feature;
	struct
	{
		const char *sysroot;
		const char *min_version;
		const char *sdk_version;
		MacSDK *sdk;
	} macos;
	struct
	{
		const char *sdk;
		const char *def;
		WinCrtLinking crt_linking;
		bool use_win_subsystem;
	} win;
	struct
	{
		const char *crt;
		const char *crtbegin;
	} linuxpaths;
} BuildTarget;

static const char *x86_cpu_set[8] = {
	[X86CPU_BASELINE] = "baseline",
	[X86CPU_SSSE3] = "ssse3",
	[X86CPU_SSE4] = "sse4",
	[X86CPU_AVX1] = "avx1",
	[X86CPU_AVX2_V1] = "avx2-v1",
	[X86CPU_AVX2_V2] = "avx2-v2",
	[X86CPU_AVX512] = "avx512",
	[X86CPU_NATIVE] = "native"
};

static BuildTarget default_build_target = {
		.optlevel = OPTIMIZATION_NOT_SET,
		.optsetting = OPT_SETTING_NOT_SET,
		.memory_environment = MEMORY_ENV_NORMAL,
		.optsize = SIZE_OPTIMIZATION_NOT_SET,
		.arch_os_target = ARCH_OS_TARGET_DEFAULT,
		.debug_info = DEBUG_INFO_NOT_SET,
		.use_stdlib = USE_STDLIB_NOT_SET,
		.link_libc = LINK_LIBC_NOT_SET,
		.emit_stdlib = EMIT_STDLIB_NOT_SET,
		.system_linker = SYSTEM_LINKER_NOT_SET,
		.single_module = SINGLE_MODULE_NOT_SET,
		.strip_unused = STRIP_UNUSED_NOT_SET,
		.symtab_size = DEFAULT_SYMTAB_SIZE,
		.reloc_model = RELOC_DEFAULT,
		.cc = "cc",
		.version = "1.0.0",
		.langrev = "1",
		.cpu = "generic",
		.type = TARGET_TYPE_EXECUTABLE,
		.feature.x86_struct_return = STRUCT_RETURN_DEFAULT,
		.feature.soft_float = SOFT_FLOAT_DEFAULT,
		.feature.fp_math = FP_DEFAULT,
		.feature.trap_on_wrap = false,
		.feature.riscv_float_capability = RISCVFLOAT_DEFAULT,
		.feature.x86_vector_capability = X86VECTOR_DEFAULT,
		.feature.x86_cpu_set = X86CPU_DEFAULT,
		.feature.safe_mode = SAFETY_NOT_SET,
		.win.crt_linking = WIN_CRT_DEFAULT,
		.win.def = NULL,
		.switchrange_max_size = DEFAULT_SWITCHRANGE_MAX_SIZE,
};

BuildOptions parse_arguments(int argc, const char *argv[]);
ArchOsTarget arch_os_target_from_string(const char *target);
bool command_accepts_files(CompilerCommand command);
void update_build_target_with_opt_level(BuildTarget *target, OptimizationSetting level);
void create_project(BuildOptions *build_options);
void create_library(BuildOptions *build_options);
