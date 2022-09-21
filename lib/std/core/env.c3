// Copyright (c) 2021 Christoffer Lerno. All rights reserved.
// Use of this source code is governed by the MIT license
// a copy of which can be found in the LICENSE_STDLIB file.
module std::core::env;

enum CompilerOptLevel
{
    O0,
    O1,
    O2,
    O3
}

enum OsType
{
	UNKNOWN,
	NONE,
	ANANAS,
	CLOUD_ABI,
	DRAGON_FLY,
	FREEBSD,
	FUCHSIA,
	IOS,
	KFREEBSD,
	LINUX,
	PS3,
	MACOSX,
	NETBSD,
	OPENBSD,
	SOLARIS,
	WIN32,
	HAIKU,
	MINIX,
	RTEMS,
	NACL,       // Native Client
	CNK,        // BG/P Compute-Node Kernel
	AIX,
	CUDA,
	NVOPENCL,
	AMDHSA,
	PS4,
	ELFIAMCU,
	TVOS,
	WATCHOS,
	MESA3D,
	CONTIKI,
	AMDPAL,
	HERMITCORE,
	HURD,
	WASI,
	EMSCRIPTEN,
}

const OsType OS_TYPE = (OsType)($$OS_TYPE);
const CompilerOptLevel COMPILER_OPT_LEVEL = (CompilerOptLevel)($$COMPILER_OPT_LEVEL);
const bool BIG_ENDIAN = $$PLATFORM_BIG_ENDIAN;
const bool I128_SUPPORT = $$PLATFORM_I128_SUPPORTED;
const bool F16_SUPPORT = $$PLATFORM_F16_SUPPORTED;
const bool F128_SUPPORT = $$PLATFORM_F128_SUPPORTED;
const bool COMPILER_SAFE_MODE = $$COMPILER_SAFE_MODE;
const usize TEMP_ALLOCATOR_SIZE = 128 * 1024;
const usize LLVM_VERSION = $$LLVM_VERSION;
const bool BENCHMARKING = $$BENCHMARKING;
const bool TESTING = $$TESTING;
