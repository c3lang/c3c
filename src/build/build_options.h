#pragma once

// Copyright (c) 2019 Christoffer Lerno. All rights reserved.
// Use of this source code is governed by the GNU LGPLv3.0 license
// a copy of which can be found in the LICENSE file.

#include "../utils/common.h"

#define MAX_LIB_DIRS 1024
#define MAX_FILES 2048

typedef enum
{
	COMMAND_MISSING = 0,
	COMMAND_COMPILE,
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
	COMPILE_OUTPUT_AST,
} CompileOption;

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
	DEBUG_INFO_NONE,
	DEBUG_INFO_LINE_TABLES,
	DEBUG_INFO_FULL
} DebugInfo;

typedef struct
{
	const char* lib_dir[MAX_LIB_DIRS];
	int lib_count;
	const char** files;
	const char* project_name;
	const char* target;
	const char* path;
	const char* cpu;
	const char* target_triple;
	struct
	{
		bool reg_struct_return : 1;
		bool stack_struct_return : 1;
		bool no_memcpy_pass : 1;
		bool soft_float : 1;
		bool no_soft_float : 1;
	} feature;
	unsigned version;
	CompilerCommand command;
	uint32_t symtab_size;
	CompileOption compile_option;
	DiagnosticsSeverity severity[DIAG_END_SENTINEL];
	OptimizationLevel optimization_level;
	SizeOptimizationLevel size_optimization_level;
	DebugInfo debug_info;
	bool debug_mode;
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
	const char **sources;
	const char **libraries;
	const char *target_triple;
} BuildTarget;

extern BuildOptions build_options;

void parse_arguments(int argc, const char *argv[]);
