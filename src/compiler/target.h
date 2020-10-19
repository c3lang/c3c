#pragma once

// Copyright (c) 2020 Christoffer Lerno. All rights reserved.
// Use of this source code is governed by a LGPLv3.0
// a copy of which can be found in the LICENSE file.


// Note: This list is based on Clang's
typedef enum
{
	ARCH_TYPE_UNKNOWN,
	ARCH_TYPE_ARM,          // ARM (little endian): arm, armv.*, xscale
	ARCH_TYPE_ARMB,         // ARM (big endian): armeb
	ARCH_TYPE_AARCH64,      // AArch64 (little endian): aarch64
	ARCH_TYPE_AARCH64_BE,   // AArch64 (big endian): aarch64_be
	ARCH_TYPE_AARCH64_32,   // AArch64 (little endian) ILP32: aarch64_32
	ARCH_TYPE_ARC,          // ARC: Synopsys ARC
	ARCH_TYPE_AVR,          // AVR: Atmel AVR microcontroller
	ARCH_TYPE_BPFEL,        // eBPF or extended BPF or 64-bit BPF (little endian)
	ARCH_TYPE_BPFEB,        // eBPF or extended BPF or 64-bit BPF (big endian)
	ARCH_TYPE_HEXAGON,      // Hexagon: hexagon
	ARCH_TYPE_MIPS,         // MIPS: mips, mipsallegrex, mipsr6
	ARCH_TYPE_MIPSEL,       // MIPSEL: mipsel, mipsallegrexe, mipsr6el
	ARCH_TYPE_MIPS64,       // MIPS64: mips64, mips64r6, mipsn32, mipsn32r6
	ARCH_TYPE_MIPS64EL,     // MIPS64EL: mips64el, mips64r6el, mipsn32el, mipsn32r6el
	ARCH_TYPE_MSP430,       // MSP430: msp430
	ARCH_TYPE_PPC,          // PPC: powerpc
	ARCH_TYPE_PPC64,        // PPC64: powerpc64, ppu
	ARCH_TYPE_PPC64LE,      // PPC64LE: powerpc64le
	ARCH_TYPE_R600,         // R600: AMD GPUs HD2XXX - HD6XXX
	ARCH_TYPE_AMDGCN,       // AMDGCN: AMD GCN GPUs
	ARCH_TYPE_RISCV32,      // RISC-V (32-bit): riscv32
	ARCH_TYPE_RISCV64,      // RISC-V (64-bit): riscv64
	ARCH_TYPE_SPARC,        // Sparc: sparc
	ARCH_TYPE_SPARCV9,      // Sparcv9: Sparcv9
	ARCH_TYPE_SPARCEL,      // Sparc: (endianness = little). NB: 'Sparcle' is a CPU variant
	ARCH_TYPE_SYSTEMZ,      // SystemZ: s390x
	ARCH_TYPE_TCE,          // TCE (http://tce.cs.tut.fi/): tce
	ARCH_TYPE_TCELE,        // TCE little endian (http://tce.cs.tut.fi/): tcele
	ARCH_TYPE_THUMB,        // Thumb (little endian): thumb, thumbv.*
	ARCH_TYPE_THUMBEB,      // Thumb (big endian): thumbeb
	ARCH_TYPE_X86,          // X86: i[3-9]86
	ARCH_TYPE_X86_64,       // X86-64: amd64, x86_64
	ARCH_TYPE_XCORE,        // XCore: xcore
	ARCH_TYPE_NVPTX,        // NVPTX: 32-bit
	ARCH_TYPE_NVPTX64,      // NVPTX: 64-bit
	ARCH_TYPE_LE32,         // le32: generic little-endian 32-bit CPU (PNaCl)
	ARCH_TYPE_LE64,         // le64: generic little-endian 64-bit CPU (PNaCl)
	ARCH_TYPE_AMDIL,        // AMDIL
	ARCH_TYPE_AMDIL64,      // AMDIL with 64-bit pointers
	ARCH_TYPE_HSAIL,        // AMD HSAIL
	ARCH_TYPE_HSAIL64,      // AMD HSAIL with 64-bit pointers
	ARCH_TYPE_SPIR,         // SPIR: standard portable IR for OpenCL 32-bit version
	ARCH_TYPE_SPIR64,       // SPIR: standard portable IR for OpenCL 64-bit version
	ARCH_TYPE_KALIMBA,      // Kalimba: generic kalimba
	ARCH_TYPE_SHAVE,        // SHAVE: Movidius vector VLIW processors
	ARCH_TYPE_LANAI,        // Lanai: Lanai 32-bit
	ARCH_TYPE_WASM32,       // WebAssembly with 32-bit pointers
	ARCH_TYPE_WASM64,       // WebAssembly with 64-bit pointers
	ARCH_TYPE_RSCRIPT32,    // 32-bit RenderScript
	ARCH_TYPE_RSCRIPT64,    // 64-bit RenderScript
	ARCH_TYPE_LAST = ARCH_TYPE_RSCRIPT64

} ArchType;

typedef enum
{
	CTYPE_SHORT,
	CTYPE_INT,
	CTYPE_LONG,
	CTYPE_LONG_LONG
} CType;

typedef enum
{
	OS_TYPE_UNKNOWN,
	OS_TYPE_ANANAS,
	OS_TYPE_CLOUD_ABI,
	OS_TYPE_DARWIN,
	OS_TYPE_DRAGON_FLY,
	OS_TYPE_FREE_BSD,
	OS_TYPE_FUCHSIA,
	OS_TYPE_IOS,
	OS_TYPE_KFREEBSD,
	OS_TYPE_LINUX,
	OS_TYPE_PS3,
	OS_TYPE_MACOSX,
	OS_TYPE_NETBSD,
	OS_TYPE_OPENBSD,
	OS_TYPE_SOLARIS,
	OS_TYPE_WIN32,
	OS_TYPE_HAIKU,
	OS_TYPE_MINIX,
	OS_TYPE_RTEMS,
	OS_TYPE_NACL,       // Native Client
	OS_TYPE_CNK,        // BG/P Compute-Node Kernel
	OS_TYPE_AIX,
	OS_TYPE_CUDA,
	OS_TYPE_NVOPENCL,
	OS_TYPE_AMDHSA,
	OS_TYPE_PS4,
	OS_TYPE_ELFIAMCU,
	OS_TYPE_TVOS,
	OS_TYPE_WATCHOS,
	OS_TYPE_MESA3D,
	OS_TYPE_CONTIKI,
	OS_TYPE_AMDPAL,
	OS_TYPE_HERMITCORE,
	OS_TYPE_HURD,
	OS_TYPE_WASI,
	OS_TYPE_EMSCRIPTEN,
	OS_TYPE_LAST = OS_TYPE_EMSCRIPTEN
} OsType;

typedef enum
{
	ENV_TYPE_UNKNOWN,
	ENV_TYPE_GNU,
	ENV_TYPE_GNUABIN32,
	ENV_TYPE_GNUABI64,
	ENV_TYPE_GNUEABI,
	ENV_TYPE_GNUEABIHF,
	ENV_TYPE_GNUX32,
	ENV_TYPE_CODE16,
	ENV_TYPE_EABI,
	ENV_TYPE_EABIHF,
	ENV_TYPE_ELFV1,
	ENV_TYPE_ELFV2,
	ENV_TYPE_ANDROID,
	ENV_TYPE_MUSL,
	ENV_TYPE_MUSLEABI,
	ENV_TYPE_MUSLEABIHF,
	ENV_TYPE_MSVC,
	ENV_TYPE_ITANIUM,
	ENV_TYPE_CYGNUS,
	ENV_TYPE_CORECLR,
	ENV_TYPE_SIMULATOR,
	ENV_TYPE_MACABI,
	ENV_TYPE_LAST = ENV_TYPE_MACABI
} EnvironmentType;

typedef enum
{
	OBJ_FORMAT_COFF,
	OBJ_FORMAT_ELF,
	OBJ_FORMAT_MACHO,
	OBJ_FORMAT_WASM,
	OBJ_FORMAT_XCOFF
} ObjectFormatType;

typedef enum
{
	VENDOR_UNKNOWN,
	VENDOR_APPLE,
	VENDOR_PC,
	VENDOR_SCEI,
	VENDOR_BGP,
	VENDOR_BGQ,
	VENDOR_FREESCALE,
	VENDOR_IBM,
	VENDOR_IMAGINATION_TECHNOLOGIES,
	VENDOR_MIPS_TECHNOLOGIES,
	VENDOR_NVIDIA,
	VENDOR_CSR,
	VENDOR_MYRIAD,
	VENDOR_AMD,
	VENDOR_MESA,
	VENDOR_SUSE,
	VENDOR_OPEN_EMBEDDED,
	VENDOR_LAST = VENDOR_OPEN_EMBEDDED
} VendorType;

typedef enum
{
	ABI_UNKNOWN,
	ABI_X64,
	ABI_WIN64,
	ABI_X86,
	ABI_AARCH64,
	ABI_WASM,
	ABI_ARM,
	ABI_PPC32,
	ABI_PPC64_SVR4,
	ABI_RISCV,
} ABI;


typedef enum
{
	FLOAT_ABI_NONE,
	FLOAT_ABI_SOFT,
	FLOAT_ABI_HARD,
} FloatABI;

typedef enum
{
	AVX_NONE,
	AVX,
	AVX_512,
} AVXLevel;

typedef enum
{
	ARM_AAPCS,
	ARM_AAPCS16,
	ARM_APCS_GNU,
	ARM_AAPCS_LINUX,
} ARMVariant;

typedef enum
{
	ARM_ABI_AAPCS,
	ARM_ABI_APCS,
	ARM_ABI_AAPCS16_VFP,
	ARM_ABI_AAPCS_VFP,
} ARMABIVariant;

typedef struct
{
	void *target;
	void *machine;
	void *llvm_data_layout;
	ArchType arch;
	const char *arch_name;
	OsType os;
	const char *os_name;
	VendorType vendor;
	const char *vendor_name;
	EnvironmentType environment_type;
	const char *environment_name;
	ObjectFormatType object_format;
	int alloca_address_space;
	ABI abi;
	FloatABI float_abi : 3;
	unsigned default_number_regs : 8;
	union
	{
		struct
		{
			bool is_darwin_vector_abi : 1;
			bool return_small_struct_in_reg_abi : 1;
			bool is_win32_float_struct_abi : 1;
			bool use_soft_float : 1;
			bool is_win_api : 1;
			bool is_mcu_api : 1;
		} x86;
		struct
		{
			AVXLevel avx_level : 3;
			bool is_win64 : 1;
			bool is_mingw64 : 1;
			bool pass_int128_vector_in_mem : 1;
		} x64;
		struct
		{
			bool is_32_bit : 1;
		} mips;
		struct
		{
			bool is_aapcs : 1;
			bool is_darwin_pcs : 1;
		} aarch64;
		struct
		{
			bool is_darwin : 1;
			bool is_win32 : 1;
		} aarch;
		struct
		{
			bool is_win32 : 1;
			ARMVariant variant : 3;
			ARMABIVariant abi_variant : 3;
		} arm;
		struct
		{
			bool is_softfp : 1;
		} ppc;
		struct
		{
			bool is_softfp : 1;
			bool is_elfv2 : 1;
			bool has_qpx : 1;
		} ppc64;
		struct
		{
			unsigned xlen;
			unsigned abiflen;
		} riscv;
		struct
		{
			bool has_vector : 1;
		} systemz;
	};
	bool little_endian;
	bool tls_supported;
	bool asm_supported;
	bool float_128;
	bool float_16;
	bool vec_128i;
	bool vec_64i;
	bool vec_128f;
	bool vec_64f;
	bool int_128;
	unsigned align_pref_pointer;
	unsigned align_pref_byte;
	unsigned align_pref_short;
	unsigned align_pref_int;
	unsigned align_pref_long;
	unsigned align_pref_i128;
	unsigned align_pref_half;
	unsigned align_pref_float;
	unsigned align_pref_double;
	unsigned align_pref_f128;
	unsigned align_pointer;
	unsigned align_byte;
	unsigned align_short;
	unsigned align_int;
	unsigned align_long;
	unsigned align_i128;
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
	unsigned width_pointer;
	unsigned width_c_short;
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
	unsigned max_size_for_return;
	char *platform_name;

} Target;

extern Target build_target;