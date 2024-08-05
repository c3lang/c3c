#pragma once
// Copyright (c) 2020-2023 Christoffer Lerno. All rights reserved.
// Use of this source code is governed by a LGPLv3.0
// a copy of which can be found in the LICENSE file.

#include "utils/lib.h"
#include "utils/json.h"
#include "build.h"

typedef struct
{
	BuildTarget **targets;
} Project;

extern const char *trust_level[3];

static const char *memory_environment[6] = {
	[MEMORY_ENV_NORMAL] = "normal",
	[MEMORY_ENV_SMALL] = "small",
	[MEMORY_ENV_TINY] = "tiny",
	[MEMORY_ENV_NONE] = "none",
};

static const char *wincrt_linking[3] = {
	[WIN_CRT_NONE] = "none",
	[WIN_CRT_DYNAMIC] = "dynamic",
	[WIN_CRT_STATIC] = "static",
};

static const char *vector_conv[2] = {
		[VECTOR_CONV_DEFAULT] = "default",
		[VECTOR_CONV_OLD] = "old",
};

static const char *optsizes[3] = {
	[SIZE_OPTIMIZATION_NONE] = "none",
	[SIZE_OPTIMIZATION_SMALL] = "small",
	[SIZE_OPTIMIZATION_TINY] = "tiny",
};

static const char *linker[3] = {
	[LINKER_TYPE_BUILTIN] = "builtin",
	[LINKER_TYPE_CC] = "cc",
	[LINKER_TYPE_CUSTOM] = "custom"
};
static const char *on_off[2] = {
	[SAFETY_OFF] = "no",
	[SAFETY_ON] = "yes",
};

static const char *riscv_capability[3] = {
	[RISCVFLOAT_NONE] = "none",
	[RISCVFLOAT_FLOAT] = "float",
	[RISCVFLOAT_DOUBLE] = "double",
};

static const char *win64_simd_type[2] = {
	[WIN64_SIMD_ARRAY] = "array",
	[WIN64_SIMD_FULL] = "full",
};

static const char *fp_math[3] = {
	[FP_STRICT] = "strict",
	[FP_RELAXED] = "relaxed",
	[FP_FAST] = "fast",
};

static const char *x86_vector_capability[6] = {
	[X86VECTOR_NONE] = "none",
	[X86VECTOR_MMX] = "mmx",
	[X86VECTOR_SSE] = "sse",
	[X86VECTOR_AVX] = "avx",
	[X86VECTOR_AVX512] = "avx512",
	[X86VECTOR_CPU] = "default"
};

static const char *optlevels[4] = {
	[OPTIMIZATION_NONE] = "none",
	[OPTIMIZATION_LESS] = "less",
	[OPTIMIZATION_MORE] = "more",
	[OPTIMIZATION_AGGRESSIVE] = "max",
};

static const char *backtrace_levels[2] = {
	[SHOW_BACKTRACE_OFF] = "off",
	[SHOW_BACKTRACE_ON] = "on",
};

static const char *reloc_models[5] = {
	[RELOC_NONE] = "none",
	[RELOC_SMALL_PIC] = "pic",
	[RELOC_BIG_PIC] = "PIC",
	[RELOC_SMALL_PIE] = "pie",
	[RELOC_BIG_PIE] = "PIE",
};

Project *project_load(void);
BuildTarget *project_select_target(Project *project, const char *optional_target);
void update_feature_flags(const char ***flags, const char ***removed_flag, const char *arg, bool add);

const char *get_string(const char *file, const char *category, JSONObject *table, const char *key,
                       const char *default_value);
int get_valid_bool(const char *file, const char *target, JSONObject *json, const char *key, int default_val);
const char *get_optional_string(const char *file, const char *category, JSONObject *table, const char *key);
const char *get_mandatory_string(const char *file, const char *category, JSONObject *object, const char *key);
const char **get_string_array(const char *file, const char *category, JSONObject *table, const char *key, bool mandatory);
const char **get_optional_string_array(const char *file, const char *target, JSONObject *table, const char *key);
const char *get_cflags(const char *file, const char *target, JSONObject *json, const char *original_flags);
void get_list_append_strings(const char *file, const char *target, JSONObject *json, const char ***list_ptr,
                             const char *base, const char *override, const char *add);
int get_valid_string_setting(const char *file, const char *target, JSONObject *json, const char *key, const char** values, int first_result, int count, const char *expected);
void check_json_keys(const char* valid_keys[][2], size_t key_count, const char* deprecated_keys[], size_t deprecated_key_count, JSONObject *json, const char *target_name, const char *option);
long get_valid_integer(JSONObject *table, const char *key, const char *category, bool mandatory);
