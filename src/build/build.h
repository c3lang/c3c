#pragma once
// Copyright (c) 2019-2023 Christoffer Lerno. All rights reserved.
// Use of this source code is governed by the GNU LGPLv3.0 license
// a copy of which can be found in the LICENSE file.

#include "../utils/lib.h"
#include "../version.h"
#include <stdint.h>

#define MAX_BUILD_LIB_DIRS 1024
#define MAX_COMMAND_LINE_FILES 4096
#define MAX_COMMAND_LINE_RUN_ARGS 2048
#define MAX_THREADS 0xFFFF
#define DEFAULT_SYMTAB_SIZE (256 * 1024)
#define DEFAULT_SWITCHRANGE_MAX_SIZE (256)
#define DEFAULT_SWITCH_JUMP_MAX_SIZE (0x3FFF)
#define DEFAULT_PATH "."

typedef enum
{
	BACKEND_LLVM = 0,
	BACKEND_TB = 1,
	BACKEND_C = 2,
} CompilerBackend;

typedef enum
{
	COMMAND_MISSING = 0,
	COMMAND_COMPILE,
	COMMAND_COMPILE_ONLY,
	COMMAND_COMPILE_BENCHMARK,
	COMMAND_COMPILE_TEST,
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
	COMMAND_BENCH,
	COMMAND_BENCHMARK,
	COMMAND_TEST,
	COMMAND_UNIT_TEST,
	COMMAND_PRINT_SYNTAX,
	COMMAND_PROJECT,
} CompilerCommand;

typedef enum
{
	SUBCOMMAND_MISSING = 0,
	SUBCOMMAND_VIEW,
	SUBCOMMAND_ADD,
	SUBCOMMAND_FETCH
} ProjectSubcommand;

typedef enum
{
	DIAG_NONE = 0,     // Don't use!
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
	LINKER_TYPE_NOT_SET = -1,
	LINKER_TYPE_BUILTIN = 0,
	LINKER_TYPE_CC = 1,
	LINKER_TYPE_CUSTOM = 2,
} LinkerType;

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
	OPTIMIZATION_NONE = 0,       // -O0
	OPTIMIZATION_LESS = 1,       // -O1
	OPTIMIZATION_MORE = 2,       // -O2
	OPTIMIZATION_AGGRESSIVE = 3, // -O3
} OptimizationLevel;

typedef enum
{
	PANIC_NOT_SET = -1,
	PANIC_OFF = 0,
	PANIC_ON = 1,
} PanicLevel;

typedef enum
{
	VALIDATION_NOT_SET = -1,
	VALIDATION_LENIENT = 0,
	VALIDATION_STRICT = 1,
	VALIDATION_OBNOXIOUS = 2,
} ValidationLevel;

typedef enum
{
	ANSI_DETECT = -1,
	ANSI_OFF = 0,
	ANSI_ON = 1
} Ansi;

typedef enum
{
	SINGLE_MODULE_NOT_SET = -1,
	SINGLE_MODULE_OFF = 0, // NOLINT
	SINGLE_MODULE_ON = 1
} SingleModule;

typedef enum
{
	UNROLL_LOOPS_NOT_SET = -1,
	UNROLL_LOOPS_OFF = 0,
	UNROLL_LOOPS_ON = 1
} UnrollLoops;

typedef enum
{
	MERGE_FUNCTIONS_NOT_SET = -1,
	MERGE_FUNCTIONS_OFF = 0,
	MERGE_FUNCTIONS_ON = 1
} MergeFunctions;

typedef enum
{
	VECTORIZATION_NOT_SET = -1,
	VECTORIZATION_OFF = 0,
	VECTORIZATION_ON = 1
} AutoVectorization;
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
	SHOW_BACKTRACE_NOT_SET = -1,
	SHOW_BACKTRACE_OFF = 0,
	SHOW_BACKTRACE_ON = 1
} ShowBacktrace;

typedef enum
{
	OLD_TEST_NOT_SET = -1,
	OLD_TEST_OFF = 0,
	OLD_TEST_ON = 1
} OldTest;

typedef enum
{
	SIZE_OPTIMIZATION_NOT_SET = -1,
	SIZE_OPTIMIZATION_NONE = 0,  // None
	SIZE_OPTIMIZATION_SMALL = 1, // -Os
	SIZE_OPTIMIZATION_TINY = 2,  // -Oz
} SizeOptimizationLevel;

typedef enum
{
	SOFT_FLOAT_DEFAULT = -1,
	SOFT_FLOAT_NONE = 0,
	SOFT_FLOAT_YES = 1
} SoftFloat;

typedef enum
{
	WIN_DEBUG_DEFAULT = -1,
	WIN_DEBUG_CODEVIEW = 0,
	WIN_DEBUG_DWARF = 1
} WinDebug;

typedef enum
{
	WIN64_SIMD_DEFAULT = -1,
	WIN64_SIMD_FULL = 0,
	WIN64_SIMD_ARRAY = 1
} Win64Simd;

typedef enum
{
	STRUCT_RETURN_DEFAULT = -1,
	STRUCT_RETURN_STACK = 0, // NOLINT
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
	X86VECTOR_CPU = 5,
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
	WIN_CRT_DYNAMIC_DEBUG = 2,
	WIN_CRT_STATIC = 3,
	WIN_CRT_STATIC_DEBUG = 4,
} WinCrtLinking;

typedef enum
{
	VECTOR_CONV_DEFAULT = 0,
	VECTOR_CONV_OLD = 1,
} VectorConv;

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
	ARCH_OS_TARGET_DEFAULT = -1,
	ANDROID_AARCH64 = 0,
	ELF_AARCH64,
	ELF_RISCV32,
	ELF_RISCV64,
	ELF_X86,
	ELF_X64,
	ELF_XTENSA,
	FREEBSD_X86,
	FREEBSD_X64,
	IOS_AARCH64,
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

typedef enum
{
	SANITIZE_NOT_SET = -1,
	SANITIZE_NONE,
	SANITIZE_ADDRESS,
	SANITIZE_MEMORY,
	SANITIZE_THREAD,
} SanitizeMode;

#define ANY_WINDOWS_ARCH_OS WINDOWS_AARCH64: case WINDOWS_X64: case MINGW_X64

typedef enum
{
	TARGET_TYPE_EXECUTABLE,
	TARGET_TYPE_STATIC_LIB,
	TARGET_TYPE_DYNAMIC_LIB,
	TARGET_TYPE_OBJECT_FILES,
	TARGET_TYPE_BENCHMARK,
	TARGET_TYPE_TEST,
	TARGET_TYPE_PREPARE,
} TargetType;

typedef enum
{
	PROJECT_VIEW_TYPE_AUTHOR,
	PROJECT_VIEW_TYPE_VERSION,
	PROJECT_VIEW_TYPE_LANGUAGE_REVISION,
	PROJECT_VIEW_TYPE_WARNINGS_USED,
	PROJECT_VIEW_TYPE_C3L_LIB_SEARCH_PATHS,
	PROJECT_VIEW_TYPE_C3L_LIB_DEPENDENCIES,
	PROJECT_VIEW_TYPE_SOURCE_PATHS,
	PROJECT_VIEW_TYPE_OUTPUT_LOCATION,
	PROJECT_VIEW_TYPE_DEFAULT_OPTIMIZATION,
	PROJECT_VIEW_TYPE_TARGETS,
	PROJECT_VIEW_TYPE_MAX = PROJECT_VIEW_TYPE_TARGETS
} ProjectViewType;

static const char *project_view_flags[PROJECT_VIEW_TYPE_MAX + 1] = {
	[PROJECT_VIEW_TYPE_AUTHOR] = "authors",
	[PROJECT_VIEW_TYPE_VERSION] = "version",
	[PROJECT_VIEW_TYPE_LANGUAGE_REVISION] = "language-revision",
	[PROJECT_VIEW_TYPE_WARNINGS_USED] = "warnings-used",
	[PROJECT_VIEW_TYPE_C3L_LIB_SEARCH_PATHS] = "c3l-lib-search-paths",
	[PROJECT_VIEW_TYPE_C3L_LIB_DEPENDENCIES] = "c3l-lib-dependencies",
	[PROJECT_VIEW_TYPE_SOURCE_PATHS] = "source-paths",
	[PROJECT_VIEW_TYPE_OUTPUT_LOCATION] = "output-location",
	[PROJECT_VIEW_TYPE_DEFAULT_OPTIMIZATION] = "default-optimization",
	[PROJECT_VIEW_TYPE_TARGETS] = "targets",
};

static const char *targets[7] = {
	[TARGET_TYPE_EXECUTABLE] = "executable",
	[TARGET_TYPE_STATIC_LIB] = "static-lib",
	[TARGET_TYPE_DYNAMIC_LIB] = "dynamic-lib",
	[TARGET_TYPE_BENCHMARK] = "benchmark",
	[TARGET_TYPE_TEST] = "test",
	[TARGET_TYPE_OBJECT_FILES] = "object-files",
	[TARGET_TYPE_PREPARE] = "prepare",
};
static const char *target_desc[7] = {
	[TARGET_TYPE_EXECUTABLE] = "Executable",
	[TARGET_TYPE_STATIC_LIB] = "Static library",
	[TARGET_TYPE_DYNAMIC_LIB] = "Dynamic library",
	[TARGET_TYPE_BENCHMARK] = "benchmark suite",
	[TARGET_TYPE_TEST] = "test suite",
	[TARGET_TYPE_OBJECT_FILES] = "object files",
	[TARGET_TYPE_PREPARE] = "prepare"

};


typedef struct BuildOptions_
{
	const char *lib_dir[MAX_BUILD_LIB_DIRS];
	size_t lib_dir_count;
	const char *libs[MAX_BUILD_LIB_DIRS];
	size_t lib_count;
	const char* linker_args[MAX_BUILD_LIB_DIRS];
	size_t linker_arg_count;
	const char* linker_lib_dir[MAX_BUILD_LIB_DIRS];
	size_t linker_lib_dir_count;
	const char* linker_libs[MAX_BUILD_LIB_DIRS];
	size_t linker_lib_count;
	const char* std_lib_dir;
	VectorConv vector_conv;
	bool enable_new_generics;
	struct
	{
		const char *sdk;
		const char *def;
		const char *vs_dirs;
		WinCrtLinking crt_linking;
	} win;
	struct
	{
		const char *sysroot;
		const char *min_version;
		const char *sdk_version;
	} macos;
	struct
	{
		const char *crt;
		const char *crtbegin;
	} linuxpaths;
	int build_threads;
	const char **libraries_to_fetch;
	const char **files;
	const char *test_filter;
	const char **args;
	const char **feature_names;
	const char **removed_feature_names;
	const char *output_name;
	const char *project_name;
	const char *target_select;
	const char *path;
	const char *vendor_download_path;
	const char *template;
	LinkerType linker_type;
	ValidationLevel validation_level;
	Ansi ansi;
	bool test_breakpoint;
	bool test_quiet;
	bool test_nosort;
	bool test_noleak;
	bool test_nocapture;
	const char *custom_linker_path;
	uint32_t symtab_size;
	unsigned version;
	bool silence_deprecation;
	CompilerBackend backend;
	CompilerCommand command;
	struct
	{
		ProjectSubcommand command;
		const char *target_name;
		TargetType target_type;
		const char **sources;

		/* Support for flags for 'view' */
		struct
		{
			uint16_t flags_bitvector;
			bool verbose;
		} view_modifier;
	} project_options;
	CompileOption compile_option;
	TrustLevel trust_level;
	DiagnosticsSeverity severity[DIAG_END_SENTINEL];
	OptimizationSetting optsetting;
	DebugInfo debug_info_override;
	ShowBacktrace show_backtrace;
	OldTest old_test;
	ArchOsTarget arch_os_target_override;
	SafetyLevel safety_level;
	PanicLevel panic_level;
	SingleModule single_module;
	UnrollLoops unroll_loops;
	MergeFunctions merge_functions;
	AutoVectorization loop_vectorization;
	AutoVectorization slp_vectorization;
	bool emit_llvm;
	bool emit_asm;
	bool benchmark_mode;
	bool test_mode;
	bool lsp_mode;
	bool no_entry;
	bool no_obj;
	bool no_headers;
	bool read_stdin;
	bool print_output;
	bool print_input;
	bool run_once;
	int verbosity_level;
	const char *panicfn;
	const char *benchfn;
	const char *testfn;
	const char *cc;
	const char *build_dir;
	const char *output_dir;
	const char *llvm_out;
	const char *asm_out;
	const char *obj_out;
	const char *script_dir;
	RelocModel reloc_model;
	X86VectorCapability x86_vector_capability;
	X86CpuSet x86_cpu_set;
	Win64Simd win_64_simd;
	WinDebug win_debug;
	FpOpt fp_math;
	EmitStdlib emit_stdlib;
	UseStdlib use_stdlib;
	LinkLibc link_libc;
	StripUnused strip_unused;
	OptimizationLevel optlevel;
	SizeOptimizationLevel optsize;
	RiscvFloatCapability riscv_float_capability;
	MemoryEnvironment memory_environment;
	SanitizeMode sanitize_mode;
	bool print_keywords;
	bool print_attributes;
	bool print_builtins;
	bool print_operators;
	bool print_type_properties;
	bool print_project_properties;
	bool print_manifest_properties;
	bool print_precedence;
	bool print_linking;
	bool benchmarking;
	bool testing;
} BuildOptions;

typedef struct
{
	struct Library__ *parent;
	ArchOsTarget arch_os;
	const char *cc;
	const char *cflags;
	WinCrtLinking win_crt;
	const char **source_dirs;
	const char **csource_dirs;
	const char **csources;
	const char **cinclude_dirs;
	const char **execs;
	const char **link_flags;
	const char **linked_libs;
	const char **dependencies;
} LibraryTarget;

typedef struct Library__
{
	const char *dir;
	const char *provides;
	const char **dependencies;
	const char **execs;
	const char *cc;
	const char *cflags;
	const char **source_dirs;
	const char **csource_dirs;
	const char **cinclude_dirs;
	WinCrtLinking win_crt;
	LibraryTarget *target_used;
	LibraryTarget **targets;
} Library;

typedef struct
{
	TargetType type;
	Library **library_list;
	LibraryTarget **ccompiling_libraries;
	const char *name;
	const char *output_name;
	const char *version;
	const char *langrev;
	const char **source_dirs;
	const char **test_source_dirs;
	const char **sources;
	const char **object_files;
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
	bool delete_after_run;
	bool generate_benchmark_runner;
	bool generate_test_runner;
	bool benchmark_output;
	bool test_output;
	bool lsp_output;
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
	bool silent;
	bool read_stdin;
	bool print_output;
	bool print_input;
	bool print_linking;
	bool no_entry;
	bool kernel_build;
	bool silence_deprecation;
	bool print_stats;
	int build_threads;
	TrustLevel trust_level;
	OptimizationSetting optsetting;
	OptimizationLevel optlevel;
	VectorConv vector_conv;
	bool enable_new_generics;
	MemoryEnvironment memory_environment;
	SizeOptimizationLevel optsize;
	SingleModule single_module;
	ValidationLevel validation_level;
	UseStdlib use_stdlib;
	EmitStdlib emit_stdlib;
	LinkLibc link_libc;
	ShowBacktrace show_backtrace;
	OldTest old_test;
	StripUnused strip_unused;
	DebugInfo debug_info;
	MergeFunctions merge_functions;
	UnrollLoops unroll_loops;
	AutoVectorization loop_vectorization;
	AutoVectorization slp_vectorization;
	RelocModel reloc_model;
	ArchOsTarget arch_os_target;
	CompilerBackend backend;
	LinkerType linker_type;
	uint32_t symtab_size;
	uint32_t switchrange_max_size;
	uint32_t switchjump_max_size;
	const char **args;
	const char *panicfn;
	const char *benchfn;
	const char *testfn;
	const char *cc;
	const char *cflags;
	const char **csource_dirs;
	const char **csources;
	const char **cinclude_dirs;
	const char **exec;
	const char **feature_list;
	const char *custom_linker_path;
	struct
	{
		WinDebug win_debug;
		SoftFloat soft_float : 3;
		StructReturn x86_struct_return : 3;
		X86VectorCapability x86_vector_capability : 4;
		RiscvFloatCapability riscv_float_capability : 4;
		Win64Simd pass_win64_simd_as_arrays : 3;
		bool trap_on_wrap : 1;
		bool sanitize_address : 1;
		bool sanitize_memory : 1;
		bool sanitize_thread : 1;
		FpOpt fp_math;
		SafetyLevel safe_mode;
		PanicLevel panic_level;
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
		const char *vs_dirs;
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
	[X86CPU_BASELINE] = "baseline", // NOLINT
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
		.show_backtrace = SHOW_BACKTRACE_NOT_SET,
		.old_test = OLD_TEST_NOT_SET,
		.use_stdlib = USE_STDLIB_NOT_SET,
		.link_libc = LINK_LIBC_NOT_SET,
		.emit_stdlib = EMIT_STDLIB_NOT_SET,
		.linker_type = LINKER_TYPE_NOT_SET,
		.validation_level = VALIDATION_NOT_SET,
		.single_module = SINGLE_MODULE_NOT_SET,
		.unroll_loops = UNROLL_LOOPS_NOT_SET,
		.merge_functions = MERGE_FUNCTIONS_NOT_SET,
		.slp_vectorization = VECTORIZATION_NOT_SET,
		.loop_vectorization = VECTORIZATION_NOT_SET,
		.strip_unused = STRIP_UNUSED_NOT_SET,
		.symtab_size = DEFAULT_SYMTAB_SIZE,
		.reloc_model = RELOC_DEFAULT,
		.cc = NULL,
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
		.feature.win_debug = WIN_DEBUG_DEFAULT,
		.feature.safe_mode = SAFETY_NOT_SET,
		.feature.panic_level = PANIC_NOT_SET,
		.win.crt_linking = WIN_CRT_DEFAULT,
		.win.def = NULL,
		.switchrange_max_size = DEFAULT_SWITCHRANGE_MAX_SIZE,
		.switchjump_max_size = DEFAULT_SWITCH_JUMP_MAX_SIZE,
};

extern const char *project_default_keys[][2];
extern const int project_default_keys_count;
extern const char *project_target_keys[][2];
extern const int project_target_keys_count;
extern const char *manifest_default_keys[][2];
extern const int manifest_default_keys_count;
extern const char *manifest_target_keys[][2];
extern const int manifest_target_keys_count;
extern const char *arch_os_target[ARCH_OS_TARGET_LAST + 1];

BuildOptions parse_arguments(int argc, const char *argv[]);
ArchOsTarget arch_os_target_from_string(const char *target);
bool command_accepts_files(CompilerCommand command);
bool command_passes_args(CompilerCommand command);
void update_build_target_with_opt_level(BuildTarget *target,
	OptimizationSetting level);
void create_project(BuildOptions *build_options);
void create_library(BuildOptions *build_options);
void resolve_libraries(BuildTarget *build_target);
void view_project(BuildOptions *build_options);
void add_target_project(BuildOptions *build_options);
void fetch_project(BuildOptions* options);
