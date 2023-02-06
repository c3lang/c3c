#pragma once

// Copyright (c) 2019 Christoffer Lerno. All rights reserved.
// Use of this source code is governed by the GNU LGPLv3.0 license
// a copy of which can be found in the LICENSE file.

#include "../utils/common.h"
#include "../version.h"

#define MAX_LIB_DIRS 1024
#define MAX_FILES 2048
#define MAX_THREADS 0xFFFF


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
	COMMAND_COMPILE_TEST,
	COMMAND_GENERATE_HEADERS,
	COMMAND_INIT,
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
	DIAG_UNREACHABLE_DEFAULT,
	DIAG_ERROR_TYPE, // Don't use this!
	DIAG_FALLOFF_NONVOID,
	DIAG_DUPLICATE_ATTRIBUTE,
	DIAG_NOT_IN_ENUM,
	DIAG_MISSING_CASE,
	DIAG_REMAINDER_DIV_BY_ZERO,
	DIAG_INT_TO_POINTER_CAST,
	DIAG_SHIFT_LHS_NEGATIVE,
	DIAG_SHIFT_NEGATIVE,
	DIAG_SHIFT_GT_TYPEWIDTH,
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
	OPT_SETTING_O0_PLUS,
	OPT_SETTING_O1,
	OPT_SETTING_O1_PLUS,
	OPT_SETTING_O2,
	OPT_SETTING_O2_PLUS,
	OPT_SETTING_O3,
	OPT_SETTING_O3_PLUS,
	OPT_SETTING_OSMALL,
	OPT_SETTING_OSMALL_PLUS,
	OPT_SETTING_OTINY,
	OPT_SETTING_OTINY_PLUS
} OptimizationSetting;

typedef enum
{
	OPTIMIZATION_NOT_SET = -1,
	OPTIMIZATION_NONE = 0,          // -O0
	OPTIMIZATION_LESS = 1,          // -O1
	OPTIMIZATION_DEFAULT = 2,       // -O2
	OPTIMIZATION_AGGRESSIVE = 3,    // -O3
} OptimizationLevel;

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
	RISCVFLOAT_DEFAULT = -1,
	RISCVFLOAT_NONE = 0,
	RISCVFLOAT_FLOAT = 1,
	RISCVFLOAT_DOUBLE = 2,
} RiscvFloatCapability;


static const char *vector_capability[6] = {
		[X86VECTOR_NONE] = "none",
		[X86VECTOR_MMX] = "mmx",
		[X86VECTOR_SSE] = "sse",
		[X86VECTOR_AVX] = "avx",
		[X86VECTOR_AVX512] = "avx512",
		[X86VECTOR_NATIVE] = "native"
};

static const char *riscv_capability[3] = {
		[RISCVFLOAT_NONE] = "none",
		[RISCVFLOAT_FLOAT] = "float",
		[RISCVFLOAT_DOUBLE] = "double",
};

typedef enum
{
	MEMORY_ENV_NOT_SET = -1,
	MEMORY_ENV_NORMAL = 0,
	MEMORY_ENV_SMALL = 1,
	MEMORY_ENV_TINY = 2,
	MEMORY_ENV_NONE = 3,
} MemoryEnvironment;

static const char *memory_environment[6] = {
		[MEMORY_ENV_NORMAL] = "normal",
		[MEMORY_ENV_SMALL] = "small",
		[MEMORY_ENV_TINY] = "tiny",
		[MEMORY_ENV_NONE] = "none",
};

typedef enum
{
	WIN_CRT_DEFAULT = -1,
	WIN_CRT_NONE = 0,
	WIN_CRT_DYNAMIC = 1,
	WIN_CRT_STATIC = 2,
} WinCrtLinking;

static const char *wincrt_linking[3] = {
		[WIN_CRT_NONE] = "none",
		[WIN_CRT_DYNAMIC] = "dynamic",
		[WIN_CRT_STATIC] = "static",
};

typedef enum
{
	RELOC_DEFAULT = -1,
	RELOC_NONE = 0,
	RELOC_SMALL_PIC = 1,
	RELOC_BIG_PIC = 2,
	RELOC_SMALL_PIE = 3,
	RELOC_BIG_PIE = 4,
} RelocModel;

static const char *reloc_models[5] = {
		[RELOC_NONE] = "none",
		[RELOC_SMALL_PIC] = "pic",
		[RELOC_BIG_PIC] = "PIC",
		[RELOC_SMALL_PIE] = "pie",
		[RELOC_BIG_PIE] = "PIE",
};

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
		WinCrtLinking crt_linking;
	} win;
	struct {
		const char *sdk;
		const char *min_version;
		const char *sdk_version;
	} macos;
	int build_threads;
	const char** libraries_to_fetch;
	const char** files;
	const char* output_name;
	const char* project_name;
	const char* target_select;
	const char* path;
	const char *template;
	uint32_t symtab_size;
	unsigned version;
	CompilerBackend backend;
	CompilerCommand command;
	CompileOption compile_option;
	DiagnosticsSeverity severity[DIAG_END_SENTINEL];
	OptimizationSetting optimization_setting_override;
	DebugInfo debug_info_override;
	ArchOsTarget arch_os_target_override;
	int safe_mode;
	bool emit_llvm;
	bool emit_asm;
	bool emit_bitcode;
	bool test_mode;
	bool no_stdlib;
	bool no_entry;
	bool no_libc;
	bool force_linker;
	bool read_stdin;
	bool print_output;
	const char *panicfn;
	const char *cc;
	const char *build_dir;
	const char *llvm_out;
	const char *asm_out;
	const char *obj_out;
	RelocModel reloc_model;
	X86VectorCapability x86_vector_capability;
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
	bool benchmarking;
	bool testing;
} BuildOptions;




typedef enum
{
	TARGET_TYPE_EXECUTABLE,
	TARGET_TYPE_STATIC_LIB,
	TARGET_TYPE_DYNAMIC_LIB,
	TARGET_TYPE_OBJECT_FILES,
	TARGET_TYPE_TEST,
} TargetType;

typedef struct
{
	ArchOsTarget arch_os;
	const char **link_flags;
	const char **linked_libs;
	const char **depends;
} LibraryTarget;

typedef struct
{
	const char *dir;
	const char *provides;
	const char **depends;
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
	const char *ir_file_dir;
	const char *asm_file_dir;
	bool run_after_compile;
	bool generate_test_runner;
	bool test_output;
	bool output_headers;
	bool output_ast;
	bool lex_only;
	bool parse_only;
	bool check_only;
	bool emit_llvm;
	bool emit_asm;
	bool no_stdlib;
	bool no_libc;
	bool emit_object_files;
	bool force_linker;
	bool benchmarking;
	bool testing;
	bool read_stdin;
	bool print_output;
	bool no_entry;
	int build_threads;
	OptimizationLevel optimization_level;
	MemoryEnvironment memory_environment;
	SizeOptimizationLevel size_optimization_level;
	bool single_module;
	DebugInfo debug_info;
	RelocModel reloc_model;
	ArchOsTarget arch_os_target;
	CompilerBackend backend;
	uint32_t symtab_size;
	uint32_t switchrange_max_size;
	const char *panicfn;
	const char *cc;
	const char *cflags;
	const char **csource_dirs;
	const char **csources;
	struct
	{
		SoftFloat soft_float : 3;
		StructReturn x86_struct_return : 3;
		X86VectorCapability x86_vector_capability : 4;
		RiscvFloatCapability riscv_float_capability : 4;
		bool trap_on_wrap : 1;
		bool safe_mode : 1;
	} feature;
	struct
	{
		const char *sdk;
		const char *min_version;
		const char *sdk_version;
	} macos;
	struct
	{
		const char *sdk;
		WinCrtLinking crt_linking;
		bool use_win_subsystem;
	} win;
} BuildTarget;


BuildOptions parse_arguments(int argc, const char *argv[]);
ArchOsTarget arch_os_target_from_string(const char *target);
bool command_is_projectless(CompilerCommand command);
void update_build_target_with_opt_level(BuildTarget *target, OptimizationSetting level);