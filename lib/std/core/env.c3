// Copyright (c) 2021-2024 Christoffer Lerno. All rights reserved.
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
	MACOS,
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
	ANDROID,
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
	XTENSA,       // Xtensa
}

const String COMPILER_BUILD_HASH = $$BUILD_HASH;
const String COMPILER_BUILD_DATE = $$BUILD_DATE;
const OsType OS_TYPE = OsType::from_ordinal($$OS_TYPE);
const ArchType ARCH_TYPE = ArchType::from_ordinal($$ARCH_TYPE);
const sz MAX_VECTOR_SIZE = $$MAX_VECTOR_SIZE;
const bool ARCH_32_BIT = $feat(ARCH_32_BIT);
const bool ARCH_64_BIT = $feat(ARCH_64_BIT);
const bool LIBC = $feat(LIBC);
const bool NO_LIBC = $feat(NO_LIBC);
const bool CUSTOM_LIBC = $feat(CUSTOM_LIBC);
const CompilerOptLevel COMPILER_OPT_LEVEL = CompilerOptLevel::from_ordinal($$COMPILER_OPT_LEVEL);
const bool BIG_ENDIAN = $feat(BIG_ENDIAN);
const bool I128_NATIVE_SUPPORT = $feat(NATIVE_I128);
const bool F16_SUPPORT = $feat(NATIVE_F16);
const bool F128_SUPPORT = $feat(NATIVE_F128);
const REGISTER_SIZE = $$REGISTER_SIZE;
const bool COMPILER_SAFE_MODE = $feat(SAFE_MODE);
const bool DEBUG_SYMBOLS = $feat(DEBUG_SYMBOLS);
const bool BACKTRACE = $feat(BACKTRACE);
const int LLVM_VERSION = $$LLVM_VERSION;
const bool BENCHMARKING = $feat(BENCHMARKING);
const bool TESTING = $feat(TESTING);
const bool PANIC_MSG = $feat(PRINT_PANIC);
const MemoryEnvironment MEMORY_ENV = MemoryEnvironment::from_ordinal($$MEMORY_ENVIRONMENT);
const bool TRACK_MEMORY = DEBUG_SYMBOLS && (COMPILER_SAFE_MODE || TESTING);
const bool X86_64 = $feat(X86_64);
const bool X86 = $feat(X86);
const bool AARCH64 = $feat(AARCH64);
const bool NATIVE_STACKTRACE = LINUX || DARWIN || OPENBSD || WIN32;
const bool LINUX = $feat(LINUX);
const bool DARWIN = $feat(DARWIN);
const bool WIN32 = $feat(WIN32);
const bool POSIX = $feat(POSIX);
const bool OPENBSD = $feat(OPENBSD);
const bool FREEBSD = $feat(FREEBSD);
const bool NETBSD = $feat(NETBSD);
const bool BSD_FAMILY = $feat(BSD);
const bool WASI = $feat(WASI);
const bool WASM = $feat(WASM);
const bool EMSCRIPTEN = $feat(EMSCRIPTEN);
const bool ANDROID = $feat(ANDROID);
const bool FREESTANDING_PE32 = $feat(FREESTANDING_PE32);
const bool FREESTANDING_MACHO = $feat(FREESTANDING_MACHO);
const bool FREESTANDING_ELF = $feat(FREESTANDING_ELF);
const bool FREESTANDING_WASM = $feat(FREESTANDING_WASM);
const bool FREESTANDING = $feat(FREESTANDING);
const bool NATIVE_THREADING = env::POSIX || env::WIN32;
const bool ADDRESS_SANITIZER = $feat(ASAN);
const bool MEMORY_SANITIZER = $feat(MSAN);
const bool THREAD_SANITIZER = $feat(TSAN);
const bool ANY_SANITIZER = ADDRESS_SANITIZER || MEMORY_SANITIZER || THREAD_SANITIZER;
const int LANGUAGE_DEV_VERSION = $$LANGUAGE_DEV_VERSION;
const bool HAS_NATIVE_ERRNO = LINUX || ANDROID || BSD_FAMILY || DARWIN || WIN32 || EMSCRIPTEN;

macro bool os_is_darwin() @const
{
	$switch OS_TYPE:
	$case IOS:
	$case MACOS:
	$case TVOS:
	$case WATCHOS:
		return true;
	$default:
		return false;
	$endswitch
}

macro bool os_is_posix() @const
{
	$switch OS_TYPE:
	$case IOS:
	$case MACOS:
	$case NETBSD:
	$case LINUX:
	$case KFREEBSD:
	$case FREEBSD:
	$case OPENBSD:
	$case SOLARIS:
	$case TVOS:
	$case WATCHOS:
	$case EMSCRIPTEN:
	$case ANDROID:
		return true;
	$case WIN32:
	$case WASI:
		return false;
	$default:
		$echo("Assuming non-Posix environment");
		return false;
	$endswitch
}
const String[] AUTHORS = $$AUTHORS;
const String[] AUTHOR_EMAILS = $$AUTHOR_EMAILS;
const String PROJECT_VERSION = $$PROJECT_VERSION;
const String PROJECT_PATH = $$PROJECT_PATH;
const BUILTIN_EXPECT_IS_DISABLED = $feat(DISABLE_BUILTIN_EXPECT);
const BUILTIN_PREFETCH_IS_DISABLED = $feat(DISABLE_BUILTIN_PREFETCH);
