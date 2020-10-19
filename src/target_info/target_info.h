#pragma once

// Copyright (c) 2020 Christoffer Lerno. All rights reserved.
// Use of this source code is governed by a LGPLv3.0
// a copy of which can be found in the LICENSE file.

#include "utils/common.h"




typedef enum
{
	SUB_ARCH_NONE,
	SUB_ARCH_ARM_V8_5A,
	SUB_ARCH_ARM_V8_4A,
	SUB_ARCH_ARM_V8_3A,
	SUB_ARCH_ARM_V8_2A,
	SUB_ARCH_ARM_V8_1A,
	SUB_ARCH_ARM_V8,
	SUB_ARCH_ARM_V8R,
	SUB_ARCH_ARM_V8M_BASELINE,
	SUB_ARCH_ARM_V8M_MAINLINE,
	SUB_ARCH_ARM_V8_1M_MAINLINE,
	SUB_ARCH_ARM_V7,
	SUB_ARCH_ARM_V7EM,
	SUB_ARCH_ARM_V7M,
	SUB_ARCH_ARM_V7S,
	SUB_ARCH_ARM_V7K,
	SUB_ARCH_ARM_V7VE,
	SUB_ARCH_ARM_V6,
	SUB_ARCH_ARM_V6M,
	SUB_ARCH_ARM_V6K,
	SUB_ARCH_ARM_V6T2,
	SUB_ARCH_ARM_V5,
	SUB_ARCH_ARM_V5TE,
	SUB_ARCH_ARM_V4,
	SUB_ARCH_KALIMBA_V3,
	SUB_ARCH_KALIMBA_V4,
	SUB_ARCH_KALIMBA_V5,
	SUB_ARCH_MIPS_V6,
} SubArchType;





typedef struct
{
	bool little_endian;
	bool tls_supported;
	bool asm_supported;
	bool float_128;
	bool float_16;
	unsigned align_pointer;
	unsigned align_byte;
	unsigned align_short;
	unsigned align_int;
	unsigned align_long;
	unsigned align_half;
	unsigned align_float;
	unsigned align_double;
	unsigned align_f128;
	unsigned align_c_long_double;
	unsigned align_c_int;
	unsigned align_c_long;
	unsigned align_c_long_long;
	unsigned align_simd_default;
	unsigned align_max_vector;
	unsigned align_global_min;
	unsigned align_new;
	unsigned align_large_array;
	unsigned width_size;
	unsigned width_pointer;
	unsigned width_c_int;
	unsigned width_c_long;
	unsigned width_c_long_long;
	unsigned width_c_long_double;
	unsigned width_c_wchar;
	unsigned width_c_wint;
	unsigned width_large_array_min;
	unsigned reg_param_max;
	unsigned sse_reg_param_max;
	unsigned builtin_ms_valist;
	unsigned aarch64sve_types;
	char *platform_name;
} TargetInfo;
