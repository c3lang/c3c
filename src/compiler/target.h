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

#define ARCH_UNSUPPORTED ARCH_TYPE_AARCH64_32: case ARCH_TYPE_BPFEL: case ARCH_TYPE_BPFEB: case ARCH_TYPE_SPARCEL: \
 case ARCH_TYPE_LE64: case ARCH_TYPE_AMDIL: case ARCH_TYPE_AMDIL64: case ARCH_TYPE_HSAIL: case ARCH_TYPE_HSAIL64:  \
 case ARCH_TYPE_KALIMBA: case ARCH_TYPE_SHAVE: case ARCH_TYPE_RSCRIPT32: case ARCH_TYPE_RSCRIPT64:                 \
 case ARCH_TYPE_LE32: case ARCH_TYPE_MIPS: case ARCH_TYPE_MIPSEL: case ARCH_TYPE_MIPS64EL: case ARCH_TYPE_MIPS64:  \
 case ARCH_TYPE_AVR: case ARCH_TYPE_NVPTX64: case ARCH_TYPE_NVPTX: case ARCH_TYPE_MSP430: case ARCH_TYPE_SYSTEMZ:  \
 case ARCH_TYPE_TCELE: case ARCH_TYPE_TCE: case ARCH_TYPE_LANAI: case ARCH_TYPE_HEXAGON: case ARCH_TYPE_AMDGCN:    \
 case ARCH_TYPE_R600: case ARCH_TYPE_SPARC: case ARCH_TYPE_SPARCV9: case ARCH_TYPE_XCORE: case ARCH_TYPE_ARC:      \
 case ARCH_TYPE_SPIR64: case ARCH_TYPE_SPIR

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
	OS_TYPE_NONE,
	OS_TYPE_ANANAS,
	OS_TYPE_CLOUD_ABI,
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

#define OS_DARWIN_TYPES OS_TYPE_WATCHOS: case OS_TYPE_IOS: case OS_TYPE_TVOS: case OS_TYPE_MACOSX
#define OS_UNSUPPORTED OS_TYPE_AIX: case OS_TYPE_HAIKU: case OS_TYPE_ANANAS: case OS_TYPE_CLOUD_ABI: \
 case OS_TYPE_DRAGON_FLY: case OS_TYPE_FUCHSIA: case OS_TYPE_KFREEBSD: case OS_TYPE_PS3: case OS_TYPE_RTEMS: \
 case OS_TYPE_SOLARIS: case OS_TYPE_MINIX: case OS_TYPE_NACL: case OS_TYPE_CNK: case OS_TYPE_CUDA:   \
 case OS_TYPE_NVOPENCL: case OS_TYPE_AMDHSA: case OS_TYPE_PS4: case OS_TYPE_ELFIAMCU: case OS_TYPE_MESA3D:   \
 case OS_TYPE_CONTIKI: case OS_TYPE_AMDPAL: case OS_TYPE_HERMITCORE: case OS_TYPE_HURD: case OS_TYPE_EMSCRIPTEN

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
	OBJ_FORMAT_UNSUPPORTED,
	OBJ_FORMAT_COFF,
	OBJ_FORMAT_GOFF,
	OBJ_FORMAT_ELF,
	OBJ_FORMAT_MACHO,
	OBJ_FORMAT_WASM,
	OBJ_FORMAT_XCOFF,
	OBJ_FORMAT_AOUT,
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
	unsigned align;
	unsigned pref_align;
} AlignData;

typedef enum
{
	BIT1,
	BITS8,
	BITS16,
	BITS32,
	BITS64,
	BITS128,
	BITS256,
	BITSIZES_LEN
} BitSizes;

typedef struct X86Features
{
	unsigned long long bits[2];
	const char *as_string;
} X86Features;

typedef struct
{
	const char *target_triple;
	int llvm_opt_level;
	const char *cpu;
	const char *features;
	ArchType arch;
	OsType os;
	VendorType vendor;
	EnvironmentType environment_type;
	ObjectFormatType object_format;
	int alloca_address_space;
	ABI abi;
	AlignData integers[BITSIZES_LEN];
	AlignData floats[BITSIZES_LEN];
	AlignData floats_pref[BITSIZES_LEN];
	RelocModel reloc_model;
	bool pic_required : 1;
	bool signed_c_char : 1;
	FloatABI float_abi : 3;
	unsigned default_number_regs_x86 : 8;
	bool use_comdat : 1;
	union
	{
		struct
		{
			bool return_small_struct_in_reg_abi : 1;
			bool use_soft_float : 1;
			bool is_mcu_api : 1;
		} x86;
		struct
		{
			X86Features features;
			unsigned align_simd_default : 16;
			bool soft_float : 1;
			bool is_win64 : 1;
			bool is_mingw64 : 1;
			bool pass_int128_vector_in_mem : 1;
			int native_vector_size_avx;
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
			unsigned flen;
		} riscv;
		struct
		{
			bool has_vector : 1;
		} systemz;
	};
	bool big_endian;
	bool tls_supported;
	bool asm_supported;
	bool float128;
	bool float16;
	bool vec128i;
	bool vec64i;
	bool vec128f;
	bool vec64f;
	bool int128;
	AlignData align_pointer;
	unsigned align_max_vector;
	unsigned align_max_tls;
	unsigned align_large_array;
	unsigned width_pointer;
	unsigned width_register;
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
	char *platform_name;
	// MinGlobalAlign is used by Clang for SystemZ and Lanai targets.

} PlatformTarget;

static inline bool is_pie_pic(RelocModel reloc)
{
	switch (reloc)
	{
		case RELOC_DEFAULT:
		case RELOC_NONE:
			return false;
		case RELOC_SMALL_PIC:
		case RELOC_BIG_PIC:
		case RELOC_SMALL_PIE:
		case RELOC_BIG_PIE:
			return true;
	}
	UNREACHABLE
}

typedef enum X64Feature
{
	X86_FEAT_ADX,
	X86_FEAT_AES,
	X86_FEAT_AMX_BF16,
	X86_FEAT_AMX_COMPLEX,
	X86_FEAT_AMX_FP16,
	X86_FEAT_AMX_INT8,
	X86_FEAT_AMX_TILE,
	X86_FEAT_AVX,
	X86_FEAT_AVX2,
	X86_FEAT_AVX5124FMAPS,
	X86_FEAT_AVX5124VNNIW,
	X86_FEAT_AVX512BF16,
	X86_FEAT_AVX512BITALG,
	X86_FEAT_AVX512BW,
	X86_FEAT_AVX512CD,
	X86_FEAT_AVX512DQ,
	X86_FEAT_AVX512ER,
	X86_FEAT_AVX512F,
	X86_FEAT_AVX512FP16,
	X86_FEAT_AVX512IFMA,
	X86_FEAT_AVX512PF,
	X86_FEAT_AVX512VBMI,
	X86_FEAT_AVX512VBMI2,
	X86_FEAT_AVX512VL,
	X86_FEAT_AVX512VNNI,
	X86_FEAT_AVX512VP2INTERSECT,
	X86_FEAT_AVX512VPOPCNTDQ,
	X86_FEAT_AVXIFMA,
	X86_FEAT_AVXNECONVERT,
	X86_FEAT_AVXVNNI,
	X86_FEAT_AVXVNNIINT16,
	X86_FEAT_AVXVNNIINT8,
	X86_FEAT_BMI,
	X86_FEAT_BMI2,
	X86_FEAT_CLDEMOTE,
	X86_FEAT_CLFLUSHOPT,
	X86_FEAT_CLWB,
	X86_FEAT_CLZERO,
	X86_FEAT_CMOV,
	X86_FEAT_CMPCCXADD,
	X86_FEAT_CMPXCHG16B,
	X86_FEAT_CMPXCHG8B,
	X86_FEAT_CRC32,
	X86_FEAT_ENQCMD,
	X86_FEAT_F16C,
	X86_FEAT_FMA,
	X86_FEAT_FMA4,
	X86_FEAT_FSGSBASE,
	X86_FEAT_FXSR,
	X86_FEAT_GFNI,
	X86_FEAT_HRESET,
	X86_FEAT_INVPCID,
	X86_FEAT_KL,
	X86_FEAT_LWP,
	X86_FEAT_LZCNT,
	X86_FEAT_MMX,
	X86_FEAT_MOVBE,
	X86_FEAT_MOVDIR64B,
	X86_FEAT_MOVDIRI,
	X86_FEAT_MWAITX,
	X86_FEAT_PCLMUL,
	X86_FEAT_PCONFIG,
	X86_FEAT_PKU,
	X86_FEAT_POPCNT,
	X86_FEAT_PREFETCHI,
	X86_FEAT_PREFETCHWT1,
	X86_FEAT_PRFCHW,
	X86_FEAT_PTWRITE,
	X86_FEAT_RAOINT,
	X86_FEAT_RDPID,
	X86_FEAT_RDPRU,
	X86_FEAT_RDRND,
	X86_FEAT_RDSEED,
	X86_FEAT_RTM,
	X86_FEAT_SAHF,
	X86_FEAT_SERIALIZE,
	X86_FEAT_SGX,
	X86_FEAT_SHA,
	X86_FEAT_SHA512,
	X86_FEAT_SHSTK,
	X86_FEAT_SM3,
	X86_FEAT_SM4,
	X86_FEAT_SSE,
	X86_FEAT_SSE2,
	X86_FEAT_SSE3,
	X86_FEAT_SSE4_1,
	X86_FEAT_SSE4_2,
	X86_FEAT_SSE4_A,
	X86_FEAT_SSSE3,
	X86_FEAT_TBM,
	X86_FEAT_TSXLDTRK,
	X86_FEAT_UINTR,
	X86_FEAT_VAES,
	X86_FEAT_VPCLMULQDQ,
	X86_FEAT_VZEROUPPER,
	X86_FEAT_WAITPKG,
	X86_FEAT_WBNOINVD,
	X86_FEAT_WIDEKL,
	X86_FEAT_X87,
	X86_FEAT_XOP,
	X86_FEAT_XSAVE,
	X86_FEAT_XSAVEC,
	X86_FEAT_XSAVEOPT,
	X86_FEAT_XSAVES,
	X86_FEATURE_LAST = X86_FEAT_XSAVES,
} X86Feature;



extern PlatformTarget platform_target;
