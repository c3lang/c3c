#pragma once
// Copyright (c) 2020-2024 Christoffer Lerno. All rights reserved.
// Use of this source code is governed by a LGPLv3.0
// a copy of which can be found in the LICENSE file.

#define MAX_ASM_INSTRUCTION_PARAMS 6
#define CLOBBER_FLAG_ELEMENTS 4
#define MAX_CLOBBER_FLAGS (64 * CLOBBER_FLAG_ELEMENTS)
#define ASM_INSTRUCTION_MAX 0x1000
#define ASM_INSTRUCTION_MASK (ASM_INSTRUCTION_MAX - 1)
#define ASM_REGISTER_MAX 4096
#define ASM_REGISTER_MASK (ASM_REGISTER_MAX - 1)

typedef struct
{
	unsigned align;
	unsigned pref_align;
} AlignData;

typedef struct
{
	bool is_write : 1;
	bool is_readwrite : 1;
	bool is_address : 1;
	AsmArgBits imm_arg_ubits : 16;
	AsmArgBits imm_arg_ibits : 16;
	AsmArgBits ireg_bits : 16;
	AsmArgBits float_bits : 16;
	AsmArgBits vec_bits : 16;
} AsmArgType;

typedef struct X86Features
{
	unsigned long long bits[2];
	const char *as_string;
} X86Features;

typedef struct
{
	uint64_t mask[CLOBBER_FLAG_ELEMENTS];
} Clobbers;

typedef struct
{
	char string[1024];
	unsigned constraint_len;
} ClobberList;

typedef struct
{
	const char *name;
	AsmRegisterType type;
	AsmArgBits bits;
	int clobber_index;
} AsmRegister;

typedef struct
{
	const char *name;
	AsmArgType param[MAX_ASM_INSTRUCTION_PARAMS];
	unsigned param_count;
	Clobbers mask;
} AsmInstruction;

typedef struct
{
	const char *target_triple;
#if LLVM_AVAILABLE
    int llvm_opt_level;
#endif
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
			bool win64_simd_as_array : 1;
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

	bool asm_initialized;
	const char **clobber_name_list;
	const char *extra_clobbers;
	AsmRegister registers[ASM_REGISTER_MAX];
	AsmInstruction instructions[ASM_INSTRUCTION_MAX];
	unsigned register_count;

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


