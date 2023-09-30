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

static const char *trust_level[3] = {
	[TRUST_NONE] = "none",
	[TRUST_INCLUDE] = "include",
	[TRUST_FULL] = "full",
};

static const char *optsizes[3] = {
	[SIZE_OPTIMIZATION_NONE] = "none",
	[SIZE_OPTIMIZATION_SMALL] = "small",
	[SIZE_OPTIMIZATION_TINY] = "tiny",
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
	[X86VECTOR_NATIVE] = "native"
};

static const char *optlevels[4] = {
	[OPTIMIZATION_NONE] = "none",
	[OPTIMIZATION_LESS] = "less",
	[OPTIMIZATION_MORE] = "more",
	[OPTIMIZATION_AGGRESSIVE] = "max",
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
