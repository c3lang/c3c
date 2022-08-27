#include <llvm-c/Target.h>
#include <llvm-c/TargetMachine.h>
#include <llvm-c/Core.h>
#include "compiler_internal.h"

static unsigned arch_pointer_bit_width(OsType os, ArchType arch);
static ArchType arch_from_llvm_string(StringSlice string);
static EnvironmentType environment_type_from_llvm_string(StringSlice string);
static bool arch_is_supported(ArchType arch);
static unsigned os_target_c_type_bits(OsType os, ArchType arch, CType type);
static AlignData os_target_alignment_of_int(OsType os, ArchType arch, uint32_t bits);
static AlignData os_target_alignment_of_float(OsType os, ArchType arch, uint32_t bits);
static OsType os_from_llvm_string(StringSlice string);
static VendorType vendor_from_llvm_string(StringSlice string);
static ObjectFormatType object_format_from_os(OsType os);
static unsigned os_target_supports_int128(OsType os, ArchType arch);
static unsigned os_target_supports_float16(OsType os, ArchType arch);
static unsigned os_target_supports_float128(OsType os, ArchType arch);
static unsigned os_target_supports_vec(OsType os, ArchType arch, int bits, bool is_int);
static bool os_requires_libc(OsType os);


PlatformTarget platform_target = { 0 };

int target_alloca_addr_space()
{
	return platform_target.alloca_address_space;
}

static void type_dump(LLVMTargetDataRef llvm_target_data, LLVMTypeRef type)
{
	unsigned long long size = LLVMSizeOfTypeInBits(llvm_target_data, type);
	unsigned abi_align = LLVMABIAlignmentOfType(llvm_target_data, type) * 8;
	unsigned pref_align = LLVMPreferredAlignmentOfType(llvm_target_data, type) * 8;

	printf(" | %-3llu  %-3d %-3d", size, abi_align, pref_align);
}


void llvm_dump(void)
{
	static char* archs[] = {
			"unknown",
			"i386",
			"aarch64",
			"arm64",
			"aarch64_be",
			"aarch64_32",
			"arm64_32",
			"arm",
			"xscale",
			"armeb",
			"xscaleeb",
			"arc",
			"avr",
			"bpfeb",
			"bpfel",
			"hexagon",
			"mips",
			"mipseb",
			"mipsallegrex",
			"mipsisa32r6",
			"mipsr6",
			"mipsel",
			"mipsallegrexel",
			"mipsisa32r6el",
			"mipsr6el",
			"mips64",
			"mips64eb",
			"mipsn32",
			"mipsisa64r6",
			"mips64r6",
			"mipsn32r6",
			"mips64el",
			"mipsn32el",
			"mipsisa64r6el",
			"mips64r6el",
			"mipsn32r6el",
			"msp430",
			"powerpc64",
			"ppu",
			"ppc64",
			"powerpc64le",
			"ppc64le",
			"powerpc",
			"ppc",
			"ppc32",
			"r600",
			"amdgcn",
			"riscv32",
			"riscv64",
			"sparc",
			"sparcel",
			"sparcv9",
			"sparc64",
			"systemz",
			"s390x",
			"tce",
			"tcele",
			"thumb",
			"thumbeb",
			"x86_64",
			"amd64",
			"x86_64h",
			"xcore",
			"nvptx",
			"nvptx64",
			"le32",
			"le64",
			"amdil",
			"amdil64",
			"hsail",
			"hsail64",
			"spir",
			"spir64",
			"kalimba",
			"lanai",
			"shave",
			"wasm32",
			"wasm64",
			"renderscript32",
			"renderscript64",
	};
	static char* os[] = {
			"unknown",
			"ananas",
			"cloudabi",
			"darwin",
			"dragonfly",
			"freebsd",
			"fuchsia",
			"ios",
			"kfreebsd",
			"linux",
			"lv2",
			"macosx",
			"netbsd",
			"openbsd",
			"solaris",
			"windows",
			"haiku",
			"minix",
			"rtems",
			"nacl",
			"cnk",
			"aix",
			"cuda",
			"nvcl",
			"amdhsa",
			"ps4",
			"elfiamcu",
			"tvos",
			"watchos",
			"mesa3d",
			"contiki",
			"amdpal",
			"hermit",
			"hurd",
			"wasi",
			"emscripten",
	};
	for (unsigned i = 0; i < sizeof(archs) / sizeof(void*); i++)
	{
		printf("----%s---\n", archs[i]);
		printf("os         end  | ptr          | i8           | i16          | i32          | i64          | i128         "
		 "| f16          | f32          | f64          | f128           "
		 "\n");
		for (unsigned j = 0; j < sizeof(os) / sizeof(void*); j++)
		{
			if ((i >= 37 && i <= 44) && (j == 3 || j == 7 || j == 11 || j == 27 || j == 28))
			{
				continue; // Skip darwin on PowerPC
			}
			LLVMTargetRef target;
			char *error;
			char *triplet = str_printf("%s-unknown-%s-unknown", archs[i], os[j]);
			if (LLVMGetTargetFromTriple(triplet, &target, &error)) continue;
			LLVMTargetMachineRef machine = NULL;
			if (!(machine = LLVMCreateTargetMachine(target, triplet, "", "", 0, LLVMRelocDefault, LLVMCodeModelDefault))) {
				error_exit("Failed to create target machine.");
			}
			LLVMTargetDataRef llvm_target_data = LLVMCreateTargetDataLayout(machine);


			printf("%-10s %-3s ", os[j],LLVMByteOrder(llvm_target_data) == LLVMBigEndian ? "BE" : "LE");
			type_dump(llvm_target_data, LLVMPointerType(LLVMInt8Type(), 0));
			type_dump(llvm_target_data, LLVMInt8Type());
			type_dump(llvm_target_data, LLVMInt16Type());
			type_dump(llvm_target_data, LLVMInt32Type());
			type_dump(llvm_target_data, LLVMInt64Type());
			type_dump(llvm_target_data, LLVMInt128Type());
			type_dump(llvm_target_data, LLVMHalfType());
			type_dump(llvm_target_data, LLVMFloatType());
			type_dump(llvm_target_data, LLVMDoubleType());
			type_dump(llvm_target_data, LLVMFP128Type());
			printf("\n");
		}
	}
}


bool os_is_apple(OsType os_type)
{
	return os_type == OS_TYPE_TVOS || os_type == OS_TYPE_WATCHOS ||
	       os_type == OS_TYPE_MACOSX || os_type == OS_TYPE_IOS;
}

static AlignSize os_arch_max_alignment_of_vector(OsType os, ArchType arch, EnvironmentType type, ARMVariant variant)
{
	switch (arch)
	{
		case ARCH_TYPE_AARCH64:
			// aarch64 uses 128 bits.
			// See Clang: AArch64TargetInfo::AArch64TargetInfo
			return 16;
		case ARCH_TYPE_ARM:
		case ARCH_TYPE_ARMB:
			if (type == ENV_TYPE_ANDROID) return 0;
			switch (variant)
			{
				case ARM_AAPCS:
				case ARM_AAPCS_LINUX:
					return 8; // AAPCS (not AAPCS16!) uses 64 bits
				case ARM_AAPCS16:
				case ARM_APCS_GNU:
					break;
			}
			break;
		case ARCH_TYPE_X86:
			if (os == OS_TYPE_WIN32) /* COFF */
			{
				return 8192;
			}
			if (os_is_apple(os))
			{
				// With AVX512 - 512, AVX - 256 otherwise AVX - 128
				return 256;
			}
			break;
		default:
			break;
	}
	// No max alignment default.
	return 0;
}

static AlignSize os_arch_max_alignment_of_tls(OsType os, ArchType arch, EnvironmentType type)
{
	switch (arch)
	{
		case ARCH_TYPE_X86:
			if (os == OS_TYPE_WIN32) /* COFF */
			{
				return 8192;
			}
			break;
		default:
			break;
	}
	// No max alignment default.
	return 0;
}

static bool os_target_use_thread_local(OsType os)
{
	switch (os)
	{
		case OS_UNSUPPORTED:
			return false;
		case OS_TYPE_UNKNOWN:
		case OS_TYPE_NONE:
			return false;
		case OS_TYPE_FREE_BSD:
		case OS_TYPE_IOS:
		case OS_TYPE_LINUX:
		case OS_TYPE_MACOSX:
		case OS_TYPE_NETBSD:
		case OS_TYPE_OPENBSD:
		case OS_TYPE_WIN32:
		case OS_TYPE_TVOS:
		case OS_TYPE_WATCHOS:
		case OS_TYPE_WASI:
			return true;
	}
	UNREACHABLE
}

static bool os_target_signed_c_char_type(OsType os, ArchType arch)
{
	switch (arch)
	{
		case ARCH_TYPE_AARCH64:
		case ARCH_TYPE_AARCH64_32:
		case ARCH_TYPE_AARCH64_BE:
		case ARCH_TYPE_ARM:
		case ARCH_TYPE_ARMB:
		case ARCH_TYPE_THUMB:
		case ARCH_TYPE_THUMBEB:
			if (os_is_apple(os) || os == OS_TYPE_WIN32) return true;
			return false;
		case ARCH_TYPE_PPC:
		case ARCH_TYPE_PPC64:
			return os_is_apple(os);
		case ARCH_TYPE_HEXAGON:
		case ARCH_TYPE_PPC64LE:
		case ARCH_TYPE_RISCV32:
		case ARCH_TYPE_RISCV64:
		case ARCH_TYPE_SYSTEMZ:
		case ARCH_TYPE_XCORE:
			return false;
		default:
			return true;
	}
}

static inline void target_setup_arm_abi(void)
{
	platform_target.abi = ABI_ARM;
	if (platform_target.os)
	{
		platform_target.arm.is_win32 = true;
		platform_target.arm.variant = ARM_AAPCS;
		platform_target.arm.abi_variant = ARM_ABI_AAPCS16_VFP;
		return;
	}
	if (platform_target.object_format == OBJ_FORMAT_MACHO)
	{
		if (platform_target.environment_type == ENV_TYPE_EABI
		    || platform_target.os == OS_TYPE_UNKNOWN /* or is M */)
		{
			platform_target.arm.variant = ARM_AAPCS;
			return;
		}
		// TODO
		if (/* is watch abi */ false)
		{
			platform_target.arm.variant = ARM_AAPCS16;
			goto SET_ABI;
		}
		platform_target.arm.variant = ARM_APCS_GNU;
		goto SET_ABI;
	}
	switch (platform_target.environment_type)
	{
		case ENV_TYPE_ANDROID:
		case ENV_TYPE_GNUEABI:
		case ENV_TYPE_GNUEABIHF:
		case ENV_TYPE_MUSLEABI:
		case ENV_TYPE_MUSLEABIHF:
			platform_target.arm.variant = ARM_AAPCS_LINUX;
			goto SET_ABI;
		case ENV_TYPE_EABI:
		case ENV_TYPE_EABIHF:
			platform_target.arm.variant = ARM_AAPCS;
			goto SET_ABI;
		case ENV_TYPE_GNU:
			platform_target.arm.variant = ARM_APCS_GNU;
			goto SET_ABI;
		default:
			break;
	}
	switch (platform_target.os)
	{
		case OS_TYPE_NETBSD:
			platform_target.arm.variant = ARM_APCS_GNU;
			break;
		case OS_TYPE_OPENBSD:
			platform_target.arm.variant = ARM_AAPCS_LINUX;
			break;
		default:
			platform_target.arm.variant = ARM_AAPCS;
			break;
	}
	SET_ABI:
	switch (platform_target.arm.variant)
	{
		case ARM_APCS_GNU:
			platform_target.arm.abi_variant = ARM_ABI_APCS;
			return;
		case ARM_AAPCS16:
			platform_target.arm.abi_variant = ARM_ABI_AAPCS16_VFP;
			return;
		case ARM_AAPCS:
		case ARM_AAPCS_LINUX:
			if (platform_target.float_abi == FLOAT_ABI_HARD ||
			    (platform_target.float_abi != FLOAT_ABI_SOFT &&
			     (platform_target.environment_type == ENV_TYPE_GNUEABIHF ||
			      platform_target.environment_type == ENV_TYPE_MUSLEABIHF ||
			      platform_target.environment_type == ENV_TYPE_EABIHF)))
			{
				platform_target.arm.abi_variant = ARM_ABI_AAPCS_VFP;
				return;
			}
			platform_target.arm.abi_variant = ARM_ABI_AAPCS;
			break;
	}
	UNREACHABLE
}

static inline void target_setup_x86_abi(BuildTarget *target)
{
	platform_target.abi = ABI_X86;
	platform_target.x86.is_win_api = platform_target.os == OS_TYPE_WIN32;
	if (os_is_apple(platform_target.os))
	{
		platform_target.x86.is_darwin_vector_abi = true;
	}
	platform_target.x86.use_soft_float = platform_target.float_abi == FLOAT_ABI_SOFT;
	// Build target override.
	if (target->feature.soft_float != SOFT_FLOAT_DEFAULT)
	{
		platform_target.x86.use_soft_float = target->feature.soft_float == SOFT_FLOAT_YES;
	}

	platform_target.x86.is_win32_float_struct_abi = platform_target.os == OS_TYPE_WIN32;
	platform_target.x86.is_mcu_api = platform_target.os == OS_TYPE_ELFIAMCU;
	if (platform_target.environment_type == ENV_TYPE_CYGNUS
	    || platform_target.environment_type == ENV_TYPE_GNU)
	{
		platform_target.x86.is_win32_float_struct_abi = false;
	}
	switch (platform_target.os)
	{
		case OS_TYPE_MACOSX:
		case OS_TYPE_IOS:
		case OS_TYPE_WATCHOS:
		case OS_TYPE_TVOS:
		case OS_TYPE_DRAGON_FLY:
		case OS_TYPE_FREE_BSD:
		case OS_TYPE_ELFIAMCU:
		case OS_TYPE_OPENBSD:
		case OS_TYPE_WIN32:
			platform_target.x86.return_small_struct_in_reg_abi = true;
			break;
		default:
			break;
	}
	if (target->feature.x86_struct_return != STRUCT_RETURN_DEFAULT)
	{
		platform_target.x86.return_small_struct_in_reg_abi = target->feature.x86_struct_return == STRUCT_RETURN_REG;
	}
}


static inline void target_setup_x64_abi(BuildTarget *target)
{
	platform_target.abi = ABI_X64;
	X86VectorCapability capability;
	platform_target.x64.is_win64 = platform_target.os == OS_TYPE_WIN32;
	if (target->feature.x86_vector_capability != X86VECTOR_DEFAULT)
	{
		capability = target->feature.x86_vector_capability;
	}
	else
	{
		capability = X86VECTOR_AVX;
	}
	platform_target.x64.x86_vector_capability = capability;
	if (target->feature.soft_float == SOFT_FLOAT_YES) platform_target.x64.soft_float = true;
	if (platform_target.environment_type == ENV_TYPE_GNU)
	{

		//platform_target.x64.is_mingw64 = platform_target.x64.is_win64;
		if (platform_target.x64.is_win64) DEBUG_LOG("Mingw");
	}
	if (platform_target.os == OS_TYPE_LINUX || platform_target.os == OS_TYPE_NETBSD)
	{
		platform_target.x64.pass_int128_vector_in_mem = true;
	}
	switch (capability)
	{
		case X86VECTOR_AVX:
			platform_target.x64.native_vector_size_avx = 32;
			platform_target.x64.align_simd_default = 256;
			break;
		case X86VECTOR_AVX512:
			platform_target.x64.native_vector_size_avx = 64;
			platform_target.x64.align_simd_default = 512;
			break;
		default:
			platform_target.x64.native_vector_size_avx = 16;
			platform_target.x64.align_simd_default = 128;
			break;
	}
}

const char *arch_to_linker_arch(ArchType arch)
{
	switch (arch)
	{
		case ARCH_UNSUPPORTED:
		case ARCH_TYPE_UNKNOWN:
			return "unknown";
		case ARCH_TYPE_ARM:
			return "arm";
		case ARCH_TYPE_ARMB:
			return "armeb";
		case ARCH_TYPE_AARCH64:
			return "aarch64";
		case ARCH_TYPE_AARCH64_BE:
			return "aarch64_be";
		case ARCH_TYPE_PPC:
			return "ppc";
		case ARCH_TYPE_PPC64:
			return "ppc64";
		case ARCH_TYPE_PPC64LE:
			return "ppc64le";
		case ARCH_TYPE_RISCV32:
			return "riscv32";
		case ARCH_TYPE_RISCV64:
			return "riscv64";
		case ARCH_TYPE_THUMB:
			return "thumb";
		case ARCH_TYPE_THUMBEB:
			return "thumbeb";
		case ARCH_TYPE_X86:
			return "i386";
		case ARCH_TYPE_X86_64:
			return "x86_64";
		case ARCH_TYPE_WASM32:
			return "wasm32";
		case ARCH_TYPE_WASM64:
			return "wasm64";
	}
	UNREACHABLE;
}

static char *arch_to_target_triple[ARCH_OS_TARGET_LAST + 1] = {
		[FREEBSD_X86] = "i386-unknown-freebsd",
		[OPENBSD_X86] = "i386-unknown-openbsd",
		[NETBSD_X86] = "i386-unknown-netbsd",
		[MCU_X86] = "i386-pc-elfiamcu",
		[WINDOWS_X86] = "i386-pc-win32",
		[LINUX_X86] = "i386-unknown-linux",
		[ELF_X86] = "i386-unknown-elf",
		[MACOS_X64] = "x86_64-apple-darwin",
		[LINUX_X64] = "x86_64-pc-linux-gnu",
		[WINDOWS_X64] = "x86_64-pc-windows-msvc",
		[MINGW_X64] = "x86_64-w64-windows-gnu",
		[NETBSD_X64] = "x86_64-pc-netbsd",
		[FREEBSD_X64] = "x86_64-pc-freebsd",
		[OPENBSD_X64] = "x86_64-pc-openbsd",
		[ELF_X64] = "x86_64-unknown-elf",
		[LINUX_AARCH64] = "aarch64-unknown-linux-gnu",
		[MACOS_AARCH64] = "aarch64-apple-darwin",
		[ELF_AARCH64] = "aarch64-unknown-elf",
		[LINUX_RISCV32] = "riscv32-unknown-linux",
		[ELF_RISCV32] = "riscv32-unknown-elf",
		[LINUX_RISCV64] = "riscv64-unknown-linux",
		[ELF_RISCV64] = "riscv64-unknown-elf",
		[WASM32] = "wasm32-unknown-unknown",
		[WASM64] = "wasm64-unknown-unknown",
};



void target_destroy()
{
}

static bool arch_is_supported(ArchType arch)
{
	switch (arch)
	{
		case ARCH_TYPE_WASM32:
		case ARCH_TYPE_WASM64:
		case ARCH_TYPE_X86_64:
		case ARCH_TYPE_AARCH64:
			return true;
		default:
			return false;
	}
}

static ArchType arch_from_llvm_string(StringSlice slice)
{
#define STRCASE(_str, _arch) if (slice_strcmp(slice, _str)) return _arch;
	STRCASE("i386", ARCH_TYPE_X86)
	STRCASE("i486", ARCH_TYPE_X86)
	STRCASE("i586", ARCH_TYPE_X86)
	STRCASE("i686", ARCH_TYPE_X86)
	STRCASE("i786", ARCH_TYPE_X86)
	STRCASE("i886", ARCH_TYPE_X86)
	STRCASE("i986", ARCH_TYPE_X86)
	STRCASE("aarch64", ARCH_TYPE_AARCH64)
	STRCASE("arm64", ARCH_TYPE_AARCH64)
	STRCASE("aarch64_be", ARCH_TYPE_AARCH64_BE)
	STRCASE("aarch64_32", ARCH_TYPE_AARCH64_32)
	STRCASE("arm64_32", ARCH_TYPE_AARCH64_32)
	STRCASE("arm", ARCH_TYPE_ARM)
	STRCASE("xscale", ARCH_TYPE_ARM)
	STRCASE("armeb", ARCH_TYPE_ARMB)
	STRCASE("xscaleeb", ARCH_TYPE_ARMB)
	STRCASE("arc", ARCH_TYPE_ARC)
	STRCASE("avr", ARCH_TYPE_AVR)
	STRCASE("bpfeb", ARCH_TYPE_BPFEB)
	STRCASE("bpfel", ARCH_TYPE_BPFEL)
	STRCASE("hexagon", ARCH_TYPE_HEXAGON)
	STRCASE("mips", ARCH_TYPE_MIPS)
	STRCASE("mipseb", ARCH_TYPE_MIPS)
	STRCASE("mipsallegrex", ARCH_TYPE_MIPS)
	STRCASE("mipsisa32r6", ARCH_TYPE_MIPS)
	STRCASE("mipsr6", ARCH_TYPE_MIPS)
	STRCASE("mipsel", ARCH_TYPE_MIPSEL)
	STRCASE("mipsallegrexel", ARCH_TYPE_MIPSEL)
	STRCASE("mipsisa32r6el", ARCH_TYPE_MIPSEL)
	STRCASE("mipsr6el", ARCH_TYPE_MIPSEL)
	STRCASE("mips64", ARCH_TYPE_MIPS64)
	STRCASE("mips64eb", ARCH_TYPE_MIPS64)
	STRCASE("mipsn32", ARCH_TYPE_MIPS64)
	STRCASE("mipsisa64r6", ARCH_TYPE_MIPS64)
	STRCASE("mips64r6", ARCH_TYPE_MIPS64)
	STRCASE("mipsn32r6", ARCH_TYPE_MIPS64)
	STRCASE("mips64el", ARCH_TYPE_MIPS64EL)
	STRCASE("mipsn32el", ARCH_TYPE_MIPS64EL)
	STRCASE("mipsisa64r6el", ARCH_TYPE_MIPS64EL)
	STRCASE("mips64r6el", ARCH_TYPE_MIPS64EL)
	STRCASE("mipsn32r6el", ARCH_TYPE_MIPS64EL)
	STRCASE("msp430", ARCH_TYPE_MSP430)
	STRCASE("powerpc64", ARCH_TYPE_PPC64)
	STRCASE("ppu", ARCH_TYPE_PPC64)
	STRCASE("ppc64", ARCH_TYPE_PPC64)
	STRCASE("powerpc64le", ARCH_TYPE_PPC64LE)
	STRCASE("ppc64le", ARCH_TYPE_PPC64LE)
	STRCASE("powerpc", ARCH_TYPE_PPC)
	STRCASE("ppc", ARCH_TYPE_PPC)
	STRCASE("ppc32", ARCH_TYPE_PPC)
	STRCASE("r600", ARCH_TYPE_R600)
	STRCASE("amdgcn", ARCH_TYPE_AMDGCN)
	STRCASE("riscv32", ARCH_TYPE_RISCV32)
	STRCASE("riscv64", ARCH_TYPE_RISCV64)
	STRCASE("sparc", ARCH_TYPE_SPARC)
	STRCASE("sparcel", ARCH_TYPE_SPARCEL)
	STRCASE("sparcv9", ARCH_TYPE_SPARCV9)
	STRCASE("sparc64", ARCH_TYPE_SPARCV9)
	STRCASE("systemz", ARCH_TYPE_SYSTEMZ)
	STRCASE("s390x", ARCH_TYPE_SYSTEMZ)
	STRCASE("tce", ARCH_TYPE_TCE)
	STRCASE("tcele", ARCH_TYPE_TCELE)
	STRCASE("thumb", ARCH_TYPE_THUMB)
	STRCASE("thumbeb", ARCH_TYPE_THUMBEB)
	STRCASE("x86_64", ARCH_TYPE_X86_64)
	STRCASE("amd64", ARCH_TYPE_X86_64)
	STRCASE("x86_64h", ARCH_TYPE_X86_64)
	STRCASE("xcore", ARCH_TYPE_XCORE)
	STRCASE("nvptx", ARCH_TYPE_NVPTX)
	STRCASE("nvptx64", ARCH_TYPE_NVPTX64)
	STRCASE("le32", ARCH_TYPE_LE32)
	STRCASE("le64", ARCH_TYPE_LE64)
	STRCASE("amdil", ARCH_TYPE_AMDIL)
	STRCASE("amdil64", ARCH_TYPE_AMDIL64)
	STRCASE("hsail", ARCH_TYPE_HSAIL)
	STRCASE("hsail64", ARCH_TYPE_HSAIL64)
	STRCASE("spir", ARCH_TYPE_SPIR)
	STRCASE("spir64", ARCH_TYPE_SPIR64)
	STRCASE("kalimba", ARCH_TYPE_KALIMBA)
	STRCASE("lanai", ARCH_TYPE_LANAI)
	STRCASE("shave", ARCH_TYPE_SHAVE)
	STRCASE("wasm32", ARCH_TYPE_WASM32)
	STRCASE("wasm64", ARCH_TYPE_WASM64)
	STRCASE("renderscript32", ARCH_TYPE_RSCRIPT32)
	STRCASE("renderscript64", ARCH_TYPE_RSCRIPT64)
	return ARCH_TYPE_UNKNOWN;
#undef STRCASE
	// TODO parse arm & bpf names
}

static EnvironmentType environment_type_from_llvm_string(StringSlice env)
{
	// Remove trailing parts.
	for (size_t i = 0; i < env.len; i++)
	{
		if (env.ptr[i] < 'A')
		{
			env.len = i;
			break;
		}
	}

#define STRCASE(_str, _arch) if (slice_strcmp(env, _str)) return _arch;
	STRCASE("gnu", ENV_TYPE_GNU)
	STRCASE("gnuabin32", ENV_TYPE_GNUABIN32)
	STRCASE("gnuabi64", ENV_TYPE_GNUABI64)
	STRCASE("gnueabihf", ENV_TYPE_GNUEABIHF)
	STRCASE("gnueabi", ENV_TYPE_GNUEABI)
	STRCASE("gnux32", ENV_TYPE_GNUX32)
	STRCASE("code16", ENV_TYPE_CODE16)
	STRCASE("eabi", ENV_TYPE_EABI)
	STRCASE("eabihf", ENV_TYPE_EABIHF)
	STRCASE("elfv1", ENV_TYPE_ELFV1)
	STRCASE("elfv2", ENV_TYPE_ELFV2)
	STRCASE("android", ENV_TYPE_ANDROID)
	STRCASE("musl", ENV_TYPE_MUSL)
	STRCASE("musleabi", ENV_TYPE_MUSLEABI)
	STRCASE("musleabihf", ENV_TYPE_MUSLEABIHF)
	STRCASE("msvc", ENV_TYPE_MSVC)
	STRCASE("itanium", ENV_TYPE_ITANIUM)
	STRCASE("cygnus", ENV_TYPE_CYGNUS)
	STRCASE("coreclr", ENV_TYPE_CORECLR)
	STRCASE("simulator", ENV_TYPE_SIMULATOR)
	STRCASE("macabi", ENV_TYPE_MACABI)
		return ENV_TYPE_UNKNOWN;
#undef STRCASE
	}

static OsType os_from_llvm_string(StringSlice os_string)
{
	// Remove trailing parts.
	for (size_t i = 0; i < os_string.len; i++)
	{
		if (os_string.ptr[i] < 'A')
		{
			os_string.len = i;
			break;
		}
	}
#define STRCASE(_str, _os) if (slice_strcmp(os_string, _str)) return _os;
	STRCASE("ananas", OS_TYPE_ANANAS)
	STRCASE("cloudabi", OS_TYPE_CLOUD_ABI)
	STRCASE("darwin", OS_TYPE_MACOSX)
	STRCASE("dragonfly", OS_TYPE_DRAGON_FLY)
	STRCASE("freebsd", OS_TYPE_FREE_BSD)
	STRCASE("fuchsia", OS_TYPE_FUCHSIA)
	STRCASE("ios", OS_TYPE_IOS)
	STRCASE("kfreebsd", OS_TYPE_KFREEBSD)
	STRCASE("linux", OS_TYPE_LINUX)
	STRCASE("lv2", OS_TYPE_PS3)
	STRCASE("macosx", OS_TYPE_MACOSX)
	STRCASE("netbsd", OS_TYPE_NETBSD)
	STRCASE("openbsd", OS_TYPE_OPENBSD)
	STRCASE("solaris", OS_TYPE_SOLARIS)
	STRCASE("windows", OS_TYPE_WIN32)
	STRCASE("haiku", OS_TYPE_HAIKU)
	STRCASE("minix", OS_TYPE_MINIX)
	STRCASE("rtems", OS_TYPE_RTEMS)
	STRCASE("nacl", OS_TYPE_NACL)
	STRCASE("cnk", OS_TYPE_CNK)
	STRCASE("aix", OS_TYPE_AIX)
	STRCASE("cuda", OS_TYPE_CUDA)
	STRCASE("nvcl", OS_TYPE_NVOPENCL)
	STRCASE("amdhsa", OS_TYPE_AMDHSA)
	STRCASE("ps4", OS_TYPE_PS4)
	STRCASE("elfiamcu", OS_TYPE_ELFIAMCU)
	STRCASE("tvos", OS_TYPE_TVOS)
	STRCASE("watchos", OS_TYPE_WATCHOS)
	STRCASE("mesa3d", OS_TYPE_MESA3D)
	STRCASE("contiki", OS_TYPE_CONTIKI)
	STRCASE("amdpal", OS_TYPE_AMDPAL)
	STRCASE("hermit", OS_TYPE_HERMITCORE)
	STRCASE("hurd", OS_TYPE_HURD)
	STRCASE("wasi", OS_TYPE_WASI)
	STRCASE("emscripten", OS_TYPE_EMSCRIPTEN)
	STRCASE("elf", OS_TYPE_NONE)
	return OS_TYPE_UNKNOWN;
#undef STRCASE
}

static VendorType vendor_from_llvm_string(StringSlice slice)
{
#define STRCASE(_str, _vendor) if (slice_strcmp(slice, _str)) return _vendor;
	STRCASE("apple", VENDOR_APPLE)
	STRCASE("pc", VENDOR_PC)
	STRCASE("scei", VENDOR_SCEI)
	STRCASE("bgp", VENDOR_BGP)
	STRCASE("bgq", VENDOR_BGQ)
	STRCASE("fsl", VENDOR_FREESCALE)
	STRCASE("ibm", VENDOR_IBM)
	STRCASE("img", VENDOR_IMAGINATION_TECHNOLOGIES)
	STRCASE("mti", VENDOR_MIPS_TECHNOLOGIES)
	STRCASE("nvidia", VENDOR_NVIDIA)
	STRCASE("csr", VENDOR_CSR)
	STRCASE("myriad", VENDOR_MYRIAD)
	STRCASE("amd", VENDOR_AMD)
	STRCASE("mesa", VENDOR_MESA)
	STRCASE("suse", VENDOR_SUSE)
	STRCASE("oe", VENDOR_OPEN_EMBEDDED)
	return VENDOR_UNKNOWN;
#undef STRCASE
}


static unsigned arch_pointer_bit_width(OsType os, ArchType arch)
{
	switch (arch)
	{
		case ARCH_TYPE_UNKNOWN:
		case ARCH_UNSUPPORTED:
			return 0;
		case ARCH_TYPE_ARM:
		case ARCH_TYPE_ARMB:
		case ARCH_TYPE_PPC:
		case ARCH_TYPE_RISCV32:
		case ARCH_TYPE_THUMB:
		case ARCH_TYPE_THUMBEB:
		case ARCH_TYPE_X86:
		case ARCH_TYPE_WASM32:
			return 32;
		case ARCH_TYPE_WASM64:
		case ARCH_TYPE_AARCH64:
		case ARCH_TYPE_AARCH64_BE:
		case ARCH_TYPE_RISCV64:
			return 64;
		case ARCH_TYPE_PPC64:
		case ARCH_TYPE_PPC64LE:
			if (os == OS_TYPE_PS3) return 32;
			return 64;
		case ARCH_TYPE_X86_64:
			if (os == OS_TYPE_NACL) return 32;
			return 64;
		default:
			UNREACHABLE
	}
}

static unsigned os_target_supports_float16(OsType os, ArchType arch)
{
	switch (arch)
	{
		case ARCH_TYPE_AARCH64:
			return true;
		case ARCH_TYPE_ARM:
			// Supported on fullfp16 and mve.fp in Clang
		default:
			// Supported by AMDGPU
			return false;
	}
}

static unsigned os_target_supports_float128(OsType os, ArchType arch)
{
	switch (arch)
	{
		case ARCH_TYPE_AARCH64:
			return true;
		case ARCH_TYPE_PPC64:
			if (os == OS_TYPE_MACOSX) return true;
			return false;
		case ARCH_TYPE_PPC:
			if (os == OS_TYPE_MACOSX) return false; // Only for later OS X 10.4+
			return false;
		case ARCH_TYPE_X86:
			if (os == OS_TYPE_MACOSX) return true;
			return false;
		default:
			return false;
	}
}

static unsigned os_target_supports_vec(OsType os, ArchType arch, int bits, bool is_int)
{
	if (bits != 64 && bits != 128) return false;
	switch (arch)
	{
		case ARCH_TYPE_AARCH64:
		case ARCH_TYPE_PPC64:
		case ARCH_TYPE_PPC:
			return true;
		case ARCH_TYPE_X86:
			if (os == OS_TYPE_MACOSX)
			{
				// 64i 128f 128i
				return bits == 128 || is_int;
			}
			return false;
		default:
			return false;
	}
}

static ObjectFormatType object_format_from_os(OsType os)
{
	switch (os)
	{
		case OS_UNSUPPORTED:
			return OBJ_FORMAT_UNSUPPORTED;
		case OS_TYPE_LINUX:
		case OS_TYPE_UNKNOWN:
		case OS_TYPE_NONE:
		case OS_TYPE_NETBSD:
		case OS_TYPE_OPENBSD:
		case OS_TYPE_FREE_BSD:
			return OBJ_FORMAT_ELF;
		case OS_TYPE_MACOSX:
		case OS_TYPE_IOS:
		case OS_TYPE_TVOS:
		case OS_TYPE_WATCHOS:
			return OBJ_FORMAT_MACHO;
		case OS_TYPE_WIN32:
			return OBJ_FORMAT_COFF;
		case OS_TYPE_WASI:
			return OBJ_FORMAT_WASM;
	}
	UNREACHABLE
}
static unsigned os_target_supports_int128(OsType os, ArchType arch)
{
	switch (arch)
	{
		case ARCH_TYPE_RISCV64:
			return true;
		case ARCH_TYPE_AARCH64:
			return true;
		case ARCH_TYPE_PPC:
		default:
			return false;
	}
}

static unsigned os_target_c_type_bits(OsType os, ArchType arch, CType type)
{
	switch (os)
	{
		case OS_UNSUPPORTED:
			UNREACHABLE
		case OS_TYPE_UNKNOWN:
			if (arch == ARCH_TYPE_MSP430)
			{
				switch (type)
				{
					case CTYPE_SHORT:
					case CTYPE_INT:
						return 16;
					case CTYPE_LONG:
						return 32;
					case CTYPE_LONG_LONG:
						return 64;
					default:
						UNREACHABLE
				}
			}
			// Use default
			break;
		case OS_TYPE_MACOSX:
		case OS_TYPE_LINUX:
		case OS_TYPE_NONE:
		case OS_TYPE_FREE_BSD:
		case OS_TYPE_NETBSD:
		case OS_TYPE_OPENBSD:
		case OS_TYPE_WASI:
			// Use default
			break;
		case OS_TYPE_WIN32:
			switch (type)
			{
				case CTYPE_SHORT:
					return 16;
				case CTYPE_INT:
				case CTYPE_LONG:
					return 32;
				case CTYPE_LONG_LONG:
					return 64;
				default:
					UNREACHABLE
			}
		case OS_TYPE_IOS:
		case OS_TYPE_TVOS:
		case OS_TYPE_WATCHOS:
			switch (type)
			{
				case CTYPE_SHORT:
					return 16;
				case CTYPE_INT:
					return 32;
				case CTYPE_LONG:
				case CTYPE_LONG_LONG:
					return 64;
				default:
					UNREACHABLE
			}
	}
	switch (type)
	{
		case CTYPE_SHORT:
			return 16;
		case CTYPE_INT:
			return 32;
		case CTYPE_LONG:
			return arch_pointer_bit_width(os, arch);
		case CTYPE_LONG_LONG:
			return 64;
		default:
			UNREACHABLE
	}

}

typedef enum {
	MACHO_MANGLING,
	ELF_MANGLING,
	MIPS_MANGLING,
	WIN86_MANGLING,
	WIN_MANGLING,
	XCOFF_MANGLING
} Mangling;


static AlignData os_target_alignment_of_int(OsType os, ArchType arch, uint32_t bits)
{
	switch (arch)
	{
		case ARCH_TYPE_UNKNOWN:
		case ARCH_UNSUPPORTED:
			UNREACHABLE
		case ARCH_TYPE_ARM:
		case ARCH_TYPE_THUMB:
		case ARCH_TYPE_ARMB:
		case ARCH_TYPE_THUMBEB:
			if ((os_is_apple(os) || os == OS_TYPE_NETBSD) && bits > 32) return (AlignData) { 32, MIN(64u, bits) };
			return (AlignData) { MIN(64u, bits), MIN(64u, bits) };
		case ARCH_TYPE_PPC64:
		case ARCH_TYPE_PPC:
		case ARCH_TYPE_PPC64LE:
		case ARCH_TYPE_X86_64:
		case ARCH_TYPE_WASM64:
		case ARCH_TYPE_RISCV32:
		case ARCH_TYPE_WASM32:
			return (AlignData) { MIN(64u, bits), MIN(64u, bits) };
		case ARCH_TYPE_RISCV64:
			return (AlignData) { bits, bits };
		case ARCH_TYPE_AARCH64:
		case ARCH_TYPE_AARCH64_BE:
			if (bits < 32 && !os_is_apple(os) && os != OS_TYPE_WIN32) return (AlignData){ bits, 32 };
			return (AlignData) { bits, bits };
		case ARCH_TYPE_X86:
			if (bits < 32) return (AlignData) { bits, bits };
			if (os == OS_TYPE_ELFIAMCU) return (AlignData) { 32, 32 };
			if (os == OS_TYPE_WIN32 || os == OS_TYPE_NACL) return (AlignData) { 64, 64 };
			return (AlignData) { 32, 64 };
	}
	UNREACHABLE
}

static unsigned arch_big_endian(ArchType arch)
{
	switch (arch)
	{
		case ARCH_TYPE_UNKNOWN:
		case ARCH_TYPE_X86:
		case ARCH_TYPE_AARCH64:
		case ARCH_TYPE_ARM:
		case ARCH_TYPE_THUMB:
		case ARCH_TYPE_PPC64LE:
		case ARCH_TYPE_X86_64:
		case ARCH_TYPE_RISCV32:
		case ARCH_TYPE_RISCV64:
		case ARCH_TYPE_WASM32:
		case ARCH_TYPE_WASM64:
			return false;
		case ARCH_TYPE_ARMB:
		case ARCH_TYPE_THUMBEB:
		case ARCH_TYPE_AARCH64_BE:
		case ARCH_TYPE_PPC64:
		case ARCH_TYPE_PPC:
			return true;
		case ARCH_UNSUPPORTED:
			UNREACHABLE
	}
	UNREACHABLE
}


static AlignData os_target_alignment_of_float(OsType os, ArchType arch, uint32_t bits)
{
	switch (arch)
	{
		case ARCH_TYPE_UNKNOWN:
		case ARCH_UNSUPPORTED:
			UNREACHABLE
		case ARCH_TYPE_X86:
			if (os == OS_TYPE_ELFIAMCU && bits >= 32) return (AlignData) { 32, 32 };
			if (os == OS_TYPE_WIN32 || os == OS_TYPE_NACL)
			{
				return (AlignData) { bits, bits };
			}
			return (AlignData) { MIN(32u, bits), bits };
		case ARCH_TYPE_AARCH64:
		case ARCH_TYPE_AARCH64_BE:
		case ARCH_TYPE_PPC64:
		case ARCH_TYPE_PPC64LE:
		case ARCH_TYPE_PPC:
		case ARCH_TYPE_RISCV32:
		case ARCH_TYPE_RISCV64:
		case ARCH_TYPE_WASM32:
		case ARCH_TYPE_WASM64:
			return (AlignData) { bits , bits };
		case ARCH_TYPE_ARM:
		case ARCH_TYPE_THUMB:
		case ARCH_TYPE_THUMBEB:
		case ARCH_TYPE_ARMB:
			if ((os_is_apple(os) || os == OS_TYPE_NETBSD) && bits == 64)
			{
				return (AlignData) { 32, bits };
			}
			return (AlignData) { bits , bits };
		case ARCH_TYPE_X86_64:
			if (bits == 128 && os == OS_TYPE_ELFIAMCU) return (AlignData) { 32, 32 };
			return (AlignData) { bits , bits };
	}
	UNREACHABLE
}

static const char *os_dynamic_library_suffix(OsType os)
{
	if (os_is_apple(os)) return ".dylib";
	if (os == OS_TYPE_WIN32) return ".dll";
	return ".so";
}

static RelocModel arch_os_reloc_default(ArchType arch, OsType os, EnvironmentType env, bool library)
{
	if (library)
	{
		switch (os)
		{
			case OS_UNSUPPORTED:
				UNREACHABLE
			case OS_TYPE_OPENBSD:
			case OS_DARWIN_TYPES:
				return RELOC_SMALL_PIC;
			case OS_TYPE_WIN32:
				return ARCH_TYPE_X86_64 == arch ? RELOC_SMALL_PIC : RELOC_NONE;
			case OS_TYPE_WASI:
				return RELOC_NONE;
			case OS_TYPE_UNKNOWN:
			case OS_TYPE_NONE:
			case OS_TYPE_FREE_BSD:
			case OS_TYPE_LINUX:
			case OS_TYPE_NETBSD:
				switch (arch)
				{
					case ARCH_TYPE_MIPS64:
					case ARCH_TYPE_MIPS64EL:
						return RELOC_SMALL_PIC;
					default:
						return RELOC_NONE;
				}
		}
		UNREACHABLE
	}
	switch (os)
	{
		case OS_TYPE_UNKNOWN:
		case OS_TYPE_NONE:
			return RELOC_NONE;
		case OS_TYPE_OPENBSD:
		case OS_TYPE_WIN32:
		case OS_DARWIN_TYPES:
		case OS_TYPE_WASI:
		case OS_TYPE_FREE_BSD:
		case OS_TYPE_NETBSD:
			return RELOC_SMALL_PIE;
		case OS_TYPE_LINUX:
			if (env == ENV_TYPE_MUSLEABI || env == ENV_TYPE_MUSLEABIHF || env == ENV_TYPE_ANDROID)
			{
				return RELOC_SMALL_PIE;
			}
			return RELOC_NONE;
		case OS_UNSUPPORTED:
			UNREACHABLE
	}
	UNREACHABLE
}

static bool arch_os_pic_default_forced(ArchType arch, OsType os)
{
	switch (os)
	{
		case OS_TYPE_WIN32:
			return arch == ARCH_TYPE_X86_64;
		case OS_DARWIN_TYPES:
			return arch == ARCH_TYPE_AARCH64 || arch == ARCH_TYPE_X86_64;
		case OS_TYPE_WASI:
		case OS_TYPE_UNKNOWN:
		case OS_TYPE_NONE:
		case OS_TYPE_FREE_BSD:
		case OS_TYPE_LINUX:
		case OS_TYPE_NETBSD:
		case OS_TYPE_OPENBSD:
			return false;
		case OS_UNSUPPORTED:
			UNREACHABLE
	}
	UNREACHABLE
}


static AlignSize os_target_pref_alignment_of_float(OsType os, ArchType arch, uint32_t bits)
{
	switch (arch)
	{
		case ARCH_TYPE_UNKNOWN:
		case ARCH_UNSUPPORTED:
			UNREACHABLE
		case ARCH_TYPE_X86:
			if (os == OS_TYPE_ELFIAMCU && bits >= 32) return 4;
			return bits / 8;
		case ARCH_TYPE_AARCH64:
		case ARCH_TYPE_AARCH64_BE:
		case ARCH_TYPE_PPC64:
		case ARCH_TYPE_PPC64LE:
		case ARCH_TYPE_PPC:
		case ARCH_TYPE_RISCV32:
		case ARCH_TYPE_RISCV64:
		case ARCH_TYPE_WASM32:
		case ARCH_TYPE_WASM64:
		case ARCH_TYPE_ARM:
		case ARCH_TYPE_THUMB:
		case ARCH_TYPE_THUMBEB:
		case ARCH_TYPE_ARMB:
			return bits / 8;
		case ARCH_TYPE_X86_64:
			if (bits == 128 && os == OS_TYPE_ELFIAMCU) return 4;
			return bits / 8;
	}
	UNREACHABLE
}


void *llvm_target_machine_create(void)
{
	char *err = NULL;
	LLVMTargetRef target = NULL;
	if (LLVMGetTargetFromTriple(platform_target.target_triple, &target, &err) != 0)
	{
		error_exit("Could not create target: %s for triple '%s'", err, platform_target.target_triple);
		// Usually we would dispose of err, but no need to do it due to exit.
	}
	LLVMRelocMode reloc_mode = LLVMRelocDefault;

	switch (platform_target.reloc_model)
	{
		case RELOC_SMALL_PIC:
		case RELOC_BIG_PIC:
		case RELOC_SMALL_PIE:
		case RELOC_BIG_PIE:
			reloc_mode = LLVMRelocPIC;
			break;
		case RELOC_NONE:
			reloc_mode = LLVMRelocDynamicNoPic;
			break;
		case RELOC_DEFAULT:
		default:
			UNREACHABLE
	}
	scratch_buffer_clear();
	if (platform_target.arch == ARCH_TYPE_X86_64)
	{
		switch (platform_target.x64.x86_vector_capability)
		{
			case X86VECTOR_DEFAULT:
				UNREACHABLE
			case X86VECTOR_NONE:
				scratch_buffer_append("-sse,-avx,-mmx,");
				break;
			case X86VECTOR_MMX:
				scratch_buffer_append("-sse,-avx,");
				break;
			case X86VECTOR_SSE:
				scratch_buffer_append("-avx,");
				break;
			case X86VECTOR_AVX:
			case X86VECTOR_AVX512:
				break;
		}
		if (platform_target.x64.soft_float) scratch_buffer_append("+soft-float,");
	}
	char *features = scratch_buffer_to_string();
	size_t len = strlen(features);
	if (len > 0) features[len - 1] = 0;
	void *result = LLVMCreateTargetMachine(target, platform_target.target_triple,
										   platform_target.cpu ? platform_target.cpu : "", features,
	                                       (LLVMCodeGenOptLevel)platform_target.llvm_opt_level,
	                                       reloc_mode, LLVMCodeModelDefault);
	if (!result) error_exit("Failed to create target machine.");
	LLVMSetTargetMachineAsmVerbosity(result, 1);
	return result;
}


#define INITIALIZE_TARGET(X) do { \
  DEBUG_LOG("Initialize target: %s.", #X); \
  LLVMInitialize ## X ## AsmParser(); \
  LLVMInitialize ## X ## AsmPrinter(); \
  LLVMInitialize ## X ## TargetInfo(); \
  LLVMInitialize ## X ## Target(); \
  LLVMInitialize ## X ## Disassembler(); \
  LLVMInitialize ## X ## TargetMC(); \
 } while(0)

void target_setup(BuildTarget *target)
{
	INITIALIZE_TARGET(ARM);
	INITIALIZE_TARGET(AArch64);
	INITIALIZE_TARGET(RISCV);
	INITIALIZE_TARGET(WebAssembly);
	INITIALIZE_TARGET(X86);
	// To support more targets, add them above.


	if (target->arch_os_target == ARCH_OS_TARGET_DEFAULT) target->arch_os_target = default_target;

	platform_target.target_triple = arch_to_target_triple[target->arch_os_target];

	platform_target.alloca_address_space = 0;

	// Create a specific target machine
	LLVMCodeGenOptLevel level;

	switch (target->optimization_level)
	{
		case OPTIMIZATION_NOT_SET:
			UNREACHABLE;
		case OPTIMIZATION_AGGRESSIVE:
			level = LLVMCodeGenLevelAggressive;
			break;
		case OPTIMIZATION_DEFAULT:
			level = LLVMCodeGenLevelDefault;
			break;
		case OPTIMIZATION_LESS:
			level = LLVMCodeGenLevelLess;
			break;
		case OPTIMIZATION_NONE:
			level = LLVMCodeGenLevelNone;
			break;
		default:
			UNREACHABLE;
	}


	DEBUG_LOG("Feature and CPU not checked.");

	platform_target.llvm_opt_level = (int)level;
	DEBUG_LOG("Triple picked was %s.", platform_target.target_triple);
	DEBUG_LOG("Default was %s.", LLVM_DEFAULT_TARGET_TRIPLE);

	LLVMTargetMachineRef machine = llvm_target_machine_create();
	char *target_triple = LLVMGetTargetMachineTriple(machine);
	platform_target.target_triple = str_copy(target_triple, strlen(target_triple));
	LLVMDisposeMessage(target_triple);
	LLVMDisposeTargetMachine(machine);

	StringSlice target_triple_string = slice_from_string(platform_target.target_triple);
	platform_target.arch = arch_from_llvm_string(slice_next_token(&target_triple_string, '-'));
	if (!arch_is_supported(platform_target.arch))
	{
		printf("WARNING! This architecture is not supported.\n");
	}
	platform_target.vendor = vendor_from_llvm_string(slice_next_token(&target_triple_string, '-'));
	platform_target.os = os_from_llvm_string(slice_next_token(&target_triple_string, '-'));
	platform_target.environment_type = environment_type_from_llvm_string(target_triple_string);

	platform_target.float_abi = false;
	// TLS should not be supported for:
	// ARM Cygwin
	// NVPTX
	platform_target.tls_supported = os_target_use_thread_local(platform_target.os);
	platform_target.big_endian = arch_big_endian(platform_target.arch);
	platform_target.width_pointer = arch_pointer_bit_width(platform_target.os, platform_target.arch);
	platform_target.alloca_address_space = 0;
	platform_target.object_format = object_format_from_os(platform_target.os);
	switch (platform_target.object_format)
	{
		case OBJ_FORMAT_COFF:
		case OBJ_FORMAT_ELF:
		case OBJ_FORMAT_WASM:
			platform_target.use_comdat = true;
			break;
		default:
			break;
	}


	platform_target.int128 = os_target_supports_int128(platform_target.os, platform_target.arch);
	platform_target.vec128f = os_target_supports_vec(platform_target.os, platform_target.arch, 128, false);
	platform_target.vec128i = os_target_supports_vec(platform_target.os, platform_target.arch, 128, true);
	platform_target.vec64f = os_target_supports_vec(platform_target.os, platform_target.arch, 64, false);
	platform_target.vec64i = os_target_supports_vec(platform_target.os, platform_target.arch, 64, true);
	platform_target.float128 = os_target_supports_float128(platform_target.os, platform_target.arch);
	platform_target.float16 = os_target_supports_float16(platform_target.os, platform_target.arch);
	for (BitSizes i = BITS8; i < BITSIZES_LEN; i++)
	{
		unsigned bits = (unsigned) (8 << (i - 1));
		platform_target.integers[i] = os_target_alignment_of_int(platform_target.os, platform_target.arch, bits);
		platform_target.floats[i] = os_target_alignment_of_float(platform_target.os, platform_target.arch, bits);
	}
	platform_target.integers[BIT1] = platform_target.integers[BITS8];

	platform_target.align_pointer = (AlignData) { platform_target.width_pointer, platform_target.width_pointer };
	platform_target.width_c_short = os_target_c_type_bits(platform_target.os, platform_target.arch, CTYPE_SHORT);
	platform_target.width_c_int = os_target_c_type_bits(platform_target.os, platform_target.arch, CTYPE_INT);
	platform_target.width_c_long = os_target_c_type_bits(platform_target.os, platform_target.arch, CTYPE_LONG);
	platform_target.width_c_long_long = os_target_c_type_bits(platform_target.os, platform_target.arch, CTYPE_LONG_LONG);
	platform_target.signed_c_char = os_target_signed_c_char_type(platform_target.os, platform_target.arch);
	/**
	 * x86-64: CMOV, CMPXCHG8B, FPU, FXSR, MMX, FXSR, SCE, SSE, SSE2
	 * x86-64-v2: (close to Nehalem) CMPXCHG16B, LAHF-SAHF, POPCNT, SSE3, SSE4.1, SSE4.2, SSSE3
     * x86-64-v3: (close to Haswell) AVX, AVX2, BMI1, BMI2, F16C, FMA, LZCNT, MOVBE, XSAVE
     * x86-64-v4: AVX512F, AVX512BW, AVX512CD, AVX512DQ, AVX512VL
	 */
	switch (platform_target.arch)
	{
		case ARCH_UNSUPPORTED:
			UNREACHABLE
			break;
		case ARCH_TYPE_AARCH64:
		case ARCH_TYPE_AARCH64_BE:
			platform_target.aarch.is_darwin = os_is_apple(platform_target.os);
			platform_target.aarch.is_win32 = platform_target.os == OS_TYPE_WIN32;
			platform_target.abi = ABI_AARCH64;
			break;
		case ARCH_TYPE_WASM32:
		case ARCH_TYPE_WASM64:
			platform_target.abi = ABI_WASM;
			break;
		case ARCH_TYPE_ARMB:
		case ARCH_TYPE_ARM:
		case ARCH_TYPE_THUMBEB:
		case ARCH_TYPE_THUMB:
			target_setup_arm_abi();
			break;
		case ARCH_TYPE_PPC:
			FATAL_ERROR("PPC32 is not supported.");
		case ARCH_TYPE_PPC64:
		case ARCH_TYPE_PPC64LE:
			if (platform_target.object_format != OBJ_FORMAT_ELF)
			{
				if (platform_target.arch == ARCH_TYPE_PPC64LE)
				{
					FATAL_ERROR("PPC64 LE non-ELF not supported.");
				}
				FATAL_ERROR("PPC64 not supported");
			}
			/** Here we need to have different codegen depending on elf version :( */
			platform_target.abi = ABI_PPC64_SVR4;
			platform_target.ppc64.is_softfp = platform_target.float_abi == FLOAT_ABI_SOFT;
			/* todo enable if elfv2 */
			platform_target.ppc64.is_elfv2 = platform_target.arch == ARCH_TYPE_PPC64LE;
			/* todo enable if elfv1-qpx */
			platform_target.ppc64.has_qpx = false;
			break;
		case ARCH_TYPE_RISCV64:
		case ARCH_TYPE_RISCV32:
			platform_target.riscv.xlen = arch_pointer_bit_width(platform_target.os, platform_target.arch) / 8; // pointer width
			platform_target.riscv.flen = 0; // ends with f / d (64)
			platform_target.abi = ABI_RISCV;
			break;
		case ARCH_TYPE_X86:
			target_setup_x86_abi(target);
			break;
		case ARCH_TYPE_X86_64:
			target_setup_x64_abi(target);
			if (platform_target.os == OS_TYPE_WIN32)
			{
				platform_target.abi = ABI_WIN64;
				break;
			}
			platform_target.abi = ABI_X64;
			break;
		case ARCH_TYPE_UNKNOWN:
			platform_target.abi = ABI_UNKNOWN;
			break;
	}
	platform_target.align_max_vector = os_arch_max_alignment_of_vector(platform_target.os, platform_target.arch, platform_target.environment_type, platform_target.arm.variant);
	platform_target.align_max_tls = os_arch_max_alignment_of_tls(platform_target.os,
	                                                             platform_target.arch,
	                                                             platform_target.environment_type);
	platform_target.reloc_model = arch_os_reloc_default(platform_target.arch,
														platform_target.os,
														platform_target.environment_type,
														active_target.type != TARGET_TYPE_EXECUTABLE);
	platform_target.pic_required = arch_os_pic_default_forced(platform_target.arch, platform_target.os);
	// Override PIC, but only if the platform does not require PIC
	if (target->reloc_model != RELOC_DEFAULT
		&& (target->reloc_model != RELOC_NONE || !platform_target.pic_required))
	{
		platform_target.reloc_model = target->reloc_model;
	}

	assert(platform_target.reloc_model != RELOC_DEFAULT);


		// TODO remove
	type_setup(&platform_target);


}

