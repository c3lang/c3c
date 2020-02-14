// Copyright (c) 2020 Christoffer Lerno. All rights reserved.
// Use of this source code is governed by a LGPLv3.0
// a copy of which can be found in the LICENSE file.

#include "target_info_internal.h"

typedef enum
{
	AVXABI_LEVEL_NONE,
	AVXABI_AVX,
	AVXABI_512
} X86AVXABILevel;

static unsigned x86avxabi_vector_size(X86AVXABILevel level)
{
	switch (level)
	{
		case AVXABI_512:
			return 512;
		case AVXABI_AVX:
			return 256;
		case AVXABI_LEVEL_NONE:
			return 128;
	}
	UNREACHABLE
}

TargetInfo target_info_new()
{
	// From the glibc documentation, on GNU systems, malloc guarantees 16-byte
	// alignment on 64-bit systems and 8-byte alignment on 32-bit systems. See
	// https://www.gnu.org/software/libc/manual/html_node/Malloc-Examples.html.
	// This alignment guarantee also applies to Windows and Android.
	/*
	if (T.isGNUEnvironment() || T.isWindowsMSVCEnvironment() || T.isAndroid())
		NewAlign = Triple.isArch64Bit() ? 128 : Triple.isArch32Bit() ? 64 : 0;
	else
		NewAlign = 0; // Infer from basic type alignment.
	*/
	TargetInfo target_info = {
		.little_endian = true,
		.asm_supported = false,
		.float_128 = false,
		.float_16 = false,
		.align_byte = 8,
		.align_c_int = 32,
		.align_c_long = 32,
		.align_c_long_long = 64,
		.align_c_long_double = 64,
		.align_f128 = 128,
		.align_large_array = 0,
		.align_global_min = 0,
		.align_new = 0,
		.align_max_vector = 0,
		.align_simd_default = 0,
		.width_pointer = 32,
		.width_c_int = 32,
		.width_c_long = 32,
		.width_c_long_long = 64,
		.width_c_long_double = 64,
		.width_large_array_min = 0,
		.width_c_wchar = 32,
		.width_c_wint = 32,
		.reg_param_max = 0,
		.sse_reg_param_max = 0,
		.builtin_ms_valist = false,
		.aarch64sve_types = false,
		.platform_name = "unknown"
	};
	return target_info;
}

