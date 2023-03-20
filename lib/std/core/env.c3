// Copyright (c) 2021 Christoffer Lerno. All rights reserved.
// Use of this source code is governed by the MIT license
// a copy of which can be found in the LICENSE_STDLIB file.
module std::core::env;
import libc;
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

enum ArchType
{
	UNKNOWN,
	ARM,          // ARM (little endian): arm, armv.*, xscale
	ARMB,         // ARM (big endian): armeb
	AARCH64,      // AArch64 (little endian): aarch64
	AARCH64_BE,   // AArch64 (big endian): aarch64_be
	AARCH64_32,   // AArch64 (little endian) ILP32: aarch64_32
	ARC,          // ARC: Synopsys ARC
	AVR,          // AVR: Atmel AVR microcontroller
	BPFEL,        // eBPF or extended BPF or 64-bit BPF (little endian)
	BPFEB,        // eBPF or extended BPF or 64-bit BPF (big endian)
	HEXAGON,      // Hexagon: hexagon
	MIPS,         // MIPS: mips, mipsallegrex, mipsr6
	MIPSEL,       // MIPSEL: mipsel, mipsallegrexe, mipsr6el
	MIPS64,       // MIPS64: mips64, mips64r6, mipsn32, mipsn32r6
	MIPS64EL,     // MIPS64EL: mips64el, mips64r6el, mipsn32el, mipsn32r6el
	MSP430,       // MSP430: msp430
	PPC,          // PPC: powerpc
	PPC64,        // PPC64: powerpc64, ppu
	PPC64LE,      // PPC64LE: powerpc64le
	R600,         // R600: AMD GPUs HD2XXX - HD6XXX
	AMDGCN,       // AMDGCN: AMD GCN GPUs
	RISCV32,      // RISC-V (32-bit): riscv32
	RISCV64,      // RISC-V (64-bit): riscv64
	SPARC,        // Sparc: sparc
	SPARCV9,      // Sparcv9: Sparcv9
	SPARCEL,      // Sparc: (endianness = little). NB: 'Sparcle' is a CPU variant
	SYSTEMZ,      // SystemZ: s390x
	TCE,          // TCE (http://tce.cs.tut.fi/): tce
	TCELE,        // TCE little endian (http://tce.cs.tut.fi/): tcele
	THUMB,        // Thumb (little endian): thumb, thumbv.*
	THUMBEB,      // Thumb (big endian): thumbeb
	X86,          // X86: i[3-9]86
	X86_64,       // X86-64: amd64, x86_64
	XCORE,        // XCore: xcore
	NVPTX,        // NVPTX: 32-bit
	NVPTX64,      // NVPTX: 64-bit
	LE32,         // le32: generic little-endian 32-bit CPU (PNaCl)
	LE64,         // le64: generic little-endian 64-bit CPU (PNaCl)
	AMDIL,        // AMDIL
	AMDIL64,      // AMDIL with 64-bit pointers
	HSAIL,        // AMD HSAIL
	HSAIL64,      // AMD HSAIL with 64-bit pointers
	SPIR,         // SPIR: standard portable IR for OpenCL 32-bit version
	SPIR64,       // SPIR: standard portable IR for OpenCL 64-bit version
	KALIMBA,      // Kalimba: generic kalimba
	SHAVE,        // SHAVE: Movidius vector VLIW processors
	LANAI,        // Lanai: Lanai 32-bit
	WASM32,       // WebAssembly with 32-bit pointers
	WASM64,       // WebAssembly with 64-bit pointers
	RSCRIPT32,    // 32-bit RenderScript
	RSCRIPT64,    // 64-bit RenderScript
}

const OsType OS_TYPE = (OsType)$$OS_TYPE;
const ArchType ARCH_TYPE = (ArchType)$$ARCH_TYPE;
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

macro bool os_is_win32()
{
	return OS_TYPE == OsType.WIN32;
}

macro bool os_is_darwin()
{
	$switch (OS_TYPE)
	$case IOS:
	$case MACOSX:
	$case TVOS:
	$case WATCHOS:
		return true;
	$default:
		return false;
	$endswitch
}

macro bool os_is_posix()
{
	$switch (OS_TYPE)
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
	$endswitch
}


/**
 * @param [&in] name
 * @require name.len > 0
 **/
fn String! get_var(String name)
{
$if (COMPILER_LIBC_AVAILABLE && OS_TYPE != OsType.WIN32)
	@pool()
	{
		ZString val = libc::getenv(name.zstr_tcopy());
		return val ? val.as_str() : SearchResult.MISSING!;
	};
$else
	return "";
$endif
}


/**
 * @param [&in] name
 * @param [&in] value
 * @require name.len > 0
 **/
fn void set_var(String name, String value, bool overwrite = true)
{
$if (COMPILER_LIBC_AVAILABLE && OS_TYPE != OsType.WIN32)
	@pool()
	{
		if (libc::setenv(name.zstr_tcopy(), value.zstr_copy(), (int)overwrite))
		{
			unreachable();
		}
	};
$endif
}

/**
 * @param [&in] name
 * @require name.len > 0
 **/
fn void clear_var(String name)
{
$if (COMPILER_LIBC_AVAILABLE && OS_TYPE != OsType.WIN32)
	@pool()
	{
		if (libc::unsetenv(name.zstr_tcopy()))
		{
			unreachable();
		}
	};
$endif
}

