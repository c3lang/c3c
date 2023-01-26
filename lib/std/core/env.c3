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

enum MemoryEnvironment
{
	NORMAL,
	SMALL,
	TINY,
	NONE
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

const OsType OS_TYPE = (OsType)$$OS_TYPE;
const bool COMPILER_LIBC_AVAILABLE = $$COMPILER_LIBC_AVAILABLE;
const CompilerOptLevel COMPILER_OPT_LEVEL = (CompilerOptLevel)$$COMPILER_OPT_LEVEL;
const bool BIG_ENDIAN = $$PLATFORM_BIG_ENDIAN;
const bool I128_NATIVE_SUPPORT = $$PLATFORM_I128_SUPPORTED;
const bool F16_SUPPORT = $$PLATFORM_F16_SUPPORTED;
const bool F128_SUPPORT = $$PLATFORM_F128_SUPPORTED;
const bool COMPILER_SAFE_MODE = $$COMPILER_SAFE_MODE;
const usz LLVM_VERSION = $$LLVM_VERSION;
const bool BENCHMARKING = $$BENCHMARKING;
const bool TESTING = $$TESTING;
const MemoryEnvironment MEMORY_ENV = (MemoryEnvironment)$$MEMORY_ENVIRONMENT;

macro bool os_is_posix()
{
	$switch (OS_TYPE):
	$case IOS:
	$case MACOSX:
	$case NETBSD:
	$case LINUX:
	$case KFREEBSD:
	$case FREEBSD:
	$case OPENBSD:
	$case SOLARIS:
	$case TVOS:
	$case WATCHOS:
		return true;
	$case WIN32:
	$case WASI:
	$case EMSCRIPTEN:
		return false;
	$default:
		$echo("Assuming non-Posix environment");
		return false;
	$endswitch;
}

