#pragma once

// Copyright (c) 2019 Christoffer Lerno. All rights reserved.
// Use of this source code is governed by the GNU LGPLv3.0 license
// a copy of which can be found in the LICENSE file.

#include "../utils/common.h"
#include "../version.h"

#define MAX_LIB_DIRS 1024
#define MAX_FILES 2048
#define MAX_THREADS 0xFFFF

#define TB_BACKEND 0

typedef enum
{
	BACKEND_LLVM,
	BACKEND_TB
} CompilerBackend;

typedef enum
{
	COMMAND_MISSING = 0,
	COMMAND_COMPILE,
	COMMAND_COMPILE_ONLY,
	COMMAND_GENERATE_HEADERS,
	COMMAND_INIT,
	COMMAND_BUILD,
	COMMAND_COMPILE_RUN,
	COMMAND_RUN,
	COMMAND_CLEAN_RUN,
	COMMAND_CLEAN,
	COMMAND_DIST,
	COMMAND_DOCS,
	COMMAND_BENCH,
	COMMAND_UNIT_TEST,
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
	OPT_SETTING_O1 = 1,
	OPT_SETTING_O2 = 2,
	OPT_SETTING_O3 = 3,
	OPT_SETTING_OSMALL = 4,
	OPT_SETTING_OTINY = 5,
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

// Values correspond to LLVM values.
typedef enum
{
	PIC_DEFAULT = -1,
	PIC_NONE = 0,
	PIC_SMALL = 1,
	PIC_BIG = 2,
} PicGeneration;

// Values correspond to LLVM values
typedef enum
{
	PIE_DEFAULT = -1,
	PIE_NONE = 0,
	PIE_SMALL = 1,
	PIE_BIG = 2,
} PieGeneration;

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
	DEBUG_INFO_NOT_SET = -1,
	DEBUG_INFO_NONE,
	DEBUG_INFO_LINE_TABLES,
	DEBUG_INFO_FULL
} DebugInfo;

typedef enum
{
	ARCH_OS_TARGET_DEFAULT = 0,
	X86_FREEBSD,
	X86_OPENBSD,
	X86_LINUX,
	X86_WINDOWS,
	X86_MCU,
	X86_ELF,
	X64_DARWIN,
	X64_LINUX,
	X64_NETBSD,
	X64_WINDOWS,
	X64_WINDOWS_GNU,
	X64_ELF,
	AARCH64_LINUX,
	AARCH64_DARWIN,
	RISCV32_LINUX,
	RISCV64_LINUX,
	WASM32,
	WASM64,
	ARCH_OS_TARGET_LAST = WASM64
} ArchOsTarget;

typedef struct BuildOptions_
{
	const char* lib_dir[MAX_LIB_DIRS];
	const char* linker_args[MAX_LIB_DIRS];
	const char* std_lib_dir;
	int lib_count;
	int linker_arg_count;
	int build_threads;
	const char** files;
	const char* output_name;
	const char* project_name;
	const char* target_select;
	const char* path;
	uint32_t symtab_size;
	unsigned version;
	CompilerBackend backend;
	CompilerCommand command;
	CompileOption compile_option;
	PieGeneration pie;
	PicGeneration pic;
	DiagnosticsSeverity severity[DIAG_END_SENTINEL];
	OptimizationSetting optimization_setting_override;
	DebugInfo debug_info_override;
	ArchOsTarget arch_os_target_override;
	int safe_mode;
	bool emit_llvm;
	bool emit_bitcode;
	bool test_mode;
} BuildOptions;




typedef enum
{
	TARGET_TYPE_EXECUTABLE,
	TARGET_TYPE_STATIC_LIB,
	TARGET_TYPE_DYNAMIC_LIB,
	TARGET_TYPE_TEST
} TargetType;

typedef struct
{
	TargetType type;
	const char *name;
	const char *version;
	const char *langrev;
	const char **source_dirs;
	const char **sources;
	const char **libraries;
	const char *cpu;
	const char **link_args;
	bool run_after_compile : 1;
	bool test_output : 1;
	bool output_headers : 1;
	bool output_ast : 1;
	bool lex_only : 1;
	bool parse_only : 1;
	bool check_only : 1;
	bool emit_llvm : 1;
	bool emit_object_files : 1;
	bool no_link : 1;
	OptimizationLevel optimization_level;
	SizeOptimizationLevel size_optimization_level;
	DebugInfo debug_info;
	PieGeneration pie;
	PicGeneration pic;
	ArchOsTarget arch_os_target;
	CompilerBackend backend;
	uint32_t symtab_size;
	uint32_t switchrange_max_size;
	const char *cc;
	const char *cflags;
	const char **csource_dirs;
	const char **csources;
	struct
	{
		SoftFloat soft_float : 3;
		StructReturn struct_return : 3;
		bool no_memcpy_pass : 1;
		bool trap_on_wrap : 1;
		bool safe_mode : 1;
		bool no_sse : 1;
		bool no_mmx : 1;
		bool no_avx : 1;
	} feature;
} BuildTarget;


BuildOptions parse_arguments(int argc, const char *argv[]);
ArchOsTarget arch_os_target_from_string(const char *target);
