#include <llvm-c/Target.h>
#include <llvm-c/TargetMachine.h>
#include <llvm-c/Core.h>
#include <target_info/target_info.h>
#include "compiler_internal.h"

static unsigned arch_pointer_bit_width(OsType os, ArchType arch);
static ArchType arch_from_llvm_string(const char *string);
static EnvironmentType environment_type_from_llvm_string(const char *string);
static bool arch_is_supported(ArchType arch);
static unsigned os_target_c_type_bits(OsType os, ArchType arch, CType type);
static unsigned os_target_alignment_of_int(OsType os, ArchType arch, int bits);
static unsigned os_target_alignment_of_float(OsType os, ArchType arch, int bits);
static OsType os_from_llvm_string(const char *string);
static VendorType vendor_from_llvm_string(const char *string);
unsigned os_target_supports_int128(OsType os, ArchType arch);
unsigned os_target_supports_float16(OsType os, ArchType arch);
unsigned os_target_supports_float128(OsType os, ArchType arch);
unsigned os_target_supports_vec(OsType os, ArchType arch, int bits, bool is_int);

Target build_target = {};

int target_alloca_addr_space()
{
	return build_target.alloca_address_space;
}

static void type_dump(LLVMTargetDataRef llvm_target_data, LLVMTypeRef type)
{
	unsigned size = LLVMSizeOfTypeInBits(llvm_target_data, type);
	unsigned abialign = LLVMABIAlignmentOfType(llvm_target_data, type) * 8;
	unsigned prefalign = LLVMPreferredAlignmentOfType(llvm_target_data, type) * 8;

	printf(" | %-3d  %-3d %-3d", size, abialign, prefalign);
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
			char *triplet = NULL;
			asprintf(&triplet, "%s-unknown-%s-unknown", archs[i], os[j]);
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


static inline bool os_is_apple(OsType os_type)
{
	return os_type == OS_TYPE_TVOS || os_type == OS_TYPE_WATCHOS || os_type == OS_TYPE_DARWIN ||
	       os_type == OS_TYPE_MACOSX || os_type == OS_TYPE_IOS;
}

static inline void target_setup_arm_abi(void)
{
	build_target.abi = ABI_ARM;
	if (build_target.os)
	{
		build_target.arm.is_win32 = true;
		build_target.arm.variant = ARM_AAPCS;
		build_target.arm.abi_variant = ARM_ABI_AAPCS16_VFP;
		return;
	}
	if (build_target.object_format == OBJ_FORMAT_MACHO)
	{
		if (build_target.environment_type == ENV_TYPE_EABI
		    || build_target.os == OS_TYPE_UNKNOWN /* or is M */)
		{
			build_target.arm.variant = ARM_AAPCS;
			return;
		}
		// TODO
		if (/* is watch abi */ false)
		{
			build_target.arm.variant = ARM_AAPCS16;
			goto SET_ABI;
		}
		build_target.arm.variant = ARM_APCS_GNU;
		goto SET_ABI;
	}
	switch (build_target.environment_type)
	{
		case ENV_TYPE_ANDROID:
		case ENV_TYPE_GNUEABI:
		case ENV_TYPE_GNUEABIHF:
		case ENV_TYPE_MUSLEABI:
		case ENV_TYPE_MUSLEABIHF:
			build_target.arm.variant = ARM_AAPCS_LINUX;
			goto SET_ABI;
		case ENV_TYPE_EABI:
		case ENV_TYPE_EABIHF:
			build_target.arm.variant = ARM_AAPCS;
			goto SET_ABI;
		case ENV_TYPE_GNU:
			build_target.arm.variant = ARM_APCS_GNU;
			goto SET_ABI;
		default:
			break;
	}
	switch (build_target.os)
	{
		case OS_TYPE_NETBSD:
			build_target.arm.variant = ARM_APCS_GNU;
			break;
		case OS_TYPE_OPENBSD:
			build_target.arm.variant = ARM_AAPCS_LINUX;
			break;
		default:
			build_target.arm.variant = ARM_AAPCS;
			break;
	}
	SET_ABI:
	switch (build_target.arm.variant)
	{
		case ARM_APCS_GNU:
			build_target.arm.abi_variant = ARM_ABI_APCS;
			return;
		case ARM_AAPCS16:
			build_target.arm.abi_variant = ARM_ABI_AAPCS16_VFP;
			return;
		case ARM_AAPCS:
		case ARM_AAPCS_LINUX:
			if (build_target.float_abi == FLOAT_ABI_HARD ||
			    (build_target.float_abi != FLOAT_ABI_SOFT &&
			     (build_target.environment_type == ENV_TYPE_GNUEABIHF ||
			      build_target.environment_type == ENV_TYPE_MUSLEABIHF ||
			      build_target.environment_type == ENV_TYPE_EABIHF)))
			{
				build_target.arm.abi_variant = ARM_ABI_AAPCS_VFP;
				return;
			}
			build_target.arm.abi_variant = ARM_ABI_AAPCS;
			break;
	}
	UNREACHABLE
}

static inline void target_setup_x86_abi(void)
{
	build_target.abi = ABI_X86;
	build_target.x86.is_win_api = build_target.os == OS_TYPE_WIN32;
	if (os_is_apple(build_target.os))
	{
		build_target.x86.is_darwin_vector_abi = true;
	}
	build_target.x86.use_soft_float = build_target.float_abi == FLOAT_ABI_SOFT;
	// Build target override.
	if (build_options.feature.soft_float) build_target.x86.use_soft_float = true;
	if (build_options.feature.no_soft_float) build_target.x86.use_soft_float = false;

	build_target.x86.is_win32_float_struct_abi = build_target.os == OS_TYPE_WIN32;
	build_target.x86.is_mcu_api = build_target.os == OS_TYPE_ELFIAMCU;
	if (build_target.environment_type == ENV_TYPE_CYGNUS
	    || build_target.environment_type == ENV_TYPE_GNU)
	{
		build_target.x86.is_win32_float_struct_abi = false;
	}
	switch (build_target.os)
	{
		case OS_TYPE_MACOSX:
		case OS_TYPE_IOS:
		case OS_TYPE_WATCHOS:
		case OS_TYPE_TVOS:
		case OS_TYPE_DARWIN:
		case OS_TYPE_DRAGON_FLY:
		case OS_TYPE_FREE_BSD:
		case OS_TYPE_ELFIAMCU:
		case OS_TYPE_OPENBSD:
		case OS_TYPE_WIN32:
			build_target.x86.return_small_struct_in_reg_abi = true;
			break;
		default:
			break;
	}
	if (build_options.feature.reg_struct_return) build_target.x86.return_small_struct_in_reg_abi = true;
	if (build_options.feature.stack_struct_return) build_target.x86.return_small_struct_in_reg_abi = false;
}


static inline void target_setup_x64_abi(void)
{
	build_target.abi = ABI_X64;
	build_target.x64.avx_level = AVX;
	build_target.x64.is_win64 = build_target.os == OS_TYPE_WIN32;
	if (build_target.environment_type == ENV_TYPE_GNU)
	{
		build_target.x64.is_mingw64 = build_target.x64.is_win64;
	}
	if (build_target.os == OS_TYPE_LINUX || build_target.os == OS_TYPE_NETBSD)
	{
		build_target.x64.pass_int128_vector_in_mem = true;
	}
}

void target_setup(void)
{
	assert(!build_target.target);

	LLVMInitializeAllTargetInfos();
	LLVMInitializeAllTargetMCs();
	LLVMInitializeAllTargets();
	LLVMInitializeAllAsmPrinters();
	LLVMInitializeAllAsmParsers();

	build_target.target = NULL;
	if (!build_options.target)
	{
		build_options.target = LLVMGetDefaultTargetTriple();
	}
	char *err = NULL;

	if (LLVMGetTargetFromTriple(build_options.target, ((LLVMTargetRef *)&build_target.target), &err) != 0)
	{
		error_exit("Could not create target: %s", err);
		// Usually we would dispose of err, but no need to do it due to exit.
	}

	build_target.alloca_address_space = 0;

	DEBUG_LOG("Target set to %s.", build_options.target);
	// Create a specific target machine
	LLVMCodeGenOptLevel level;
	LLVMRelocMode reloc_mode = LLVMRelocDefault;

	switch (build_options.optimization_level)
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
//	reloc = (opt->pic || opt->library)? LLVMRelocPIC : LLVMRelocDefault;
	if (!build_options.cpu)
	{
		build_options.cpu = "generic";
	}
	/*
	if (!opt->features)
	{
		opt->features = "";
	}*/
	if (!(build_target.machine = LLVMCreateTargetMachine(build_target.target, build_options.target, "", "", level, reloc_mode,
	                                                     LLVMCodeModelDefault))) {
		error_exit("Failed to create target machine.");
	}

	build_target.llvm_data_layout = LLVMCreateTargetDataLayout(build_target.machine);

	char *target_triple = LLVMGetTargetMachineTriple(build_target.machine);

	build_target.arch_name = strdup(strtok(target_triple, "-"));
	build_target.vendor_name = strdup(strtok(NULL, "-"));
	build_target.os_name = strdup(strtok(NULL, "-"));
	char *env = strtok(NULL, "0123456789");
	build_target.environment_name = env ? strdup(env) : "unknown";

	LLVMDisposeMessage(target_triple);

	build_target.arch = arch_from_llvm_string(build_target.arch_name);
	if (!arch_is_supported(build_target.arch))
	{
		printf("WARNING! This architecture is not supported.\n");
	}
	build_target.environment_type = environment_type_from_llvm_string(build_target.environment_name);
	build_target.os = os_from_llvm_string(build_target.os_name);
	build_target.vendor = vendor_from_llvm_string(build_target.vendor_name);
	build_target.float_abi = false;
	build_target.width_pointer = arch_pointer_bit_width(build_target.os, build_target.arch);
	assert(build_target.width_pointer == LLVMPointerSize(build_target.llvm_data_layout) * 8);
	build_target.alloca_address_space = 0;

	switch (build_target.arch)
	{
		case ARCH_TYPE_X86_64:
		case ARCH_TYPE_X86:
		case ARCH_TYPE_PPC64LE:
		case ARCH_TYPE_ARM:
		case ARCH_TYPE_RISCV32:
		case ARCH_TYPE_RISCV64:
			build_target.max_size_for_return = build_target.width_pointer * 2 / 8;
			break;
		case ARCH_TYPE_PPC64:
			build_target.max_size_for_return = 0;
			break;
		default:
			FATAL_ERROR("Unsupported architecture.");
			break;
	}

	build_target.int_128 = os_target_supports_int128(build_target.os, build_target.arch);
	build_target.vec_128f = os_target_supports_vec(build_target.os, build_target.arch, 128, false);
	build_target.vec_128i = os_target_supports_vec(build_target.os, build_target.arch, 128, true);
	build_target.vec_64f = os_target_supports_vec(build_target.os, build_target.arch, 64, false);
	build_target.vec_64i = os_target_supports_vec(build_target.os, build_target.arch, 64, true);
	build_target.float_128 = os_target_supports_float128(build_target.os, build_target.arch);
	build_target.float_16 = os_target_supports_float16(build_target.os, build_target.arch);
	build_target.align_byte = os_target_alignment_of_int(build_target.os, build_target.arch, 8);
	build_target.align_short = os_target_alignment_of_int(build_target.os, build_target.arch, 16);
	build_target.align_int = os_target_alignment_of_int(build_target.os, build_target.arch, 32);
	build_target.align_long = os_target_alignment_of_int(build_target.os, build_target.arch, 64);
	build_target.align_i128 = os_target_alignment_of_int(build_target.os, build_target.arch, 128);
	build_target.align_half = os_target_alignment_of_float(build_target.os, build_target.arch, 16);
	build_target.align_float = os_target_alignment_of_float(build_target.os, build_target.arch, 32);
	build_target.align_double = os_target_alignment_of_float(build_target.os, build_target.arch, 64);
	build_target.align_f128 = os_target_alignment_of_float(build_target.os, build_target.arch, 128);
	build_target.align_int = os_target_alignment_of_int(build_target.os, build_target.arch, 32);
	build_target.align_pointer = build_target.width_pointer / 8;
	build_target.little_endian = LLVMByteOrder(build_target.llvm_data_layout) == LLVMLittleEndian;
	build_target.width_c_short = os_target_c_type_bits(build_target.os, build_target.arch, CTYPE_SHORT);
	build_target.width_c_int = os_target_c_type_bits(build_target.os, build_target.arch, CTYPE_INT);
	build_target.width_c_long = os_target_c_type_bits(build_target.os, build_target.arch, CTYPE_LONG);
	build_target.width_c_long_long = os_target_c_type_bits(build_target.os, build_target.arch, CTYPE_LONG_LONG);

	switch (build_target.arch)
	{
		case ARCH_TYPE_AARCH64_32:
		case ARCH_TYPE_BPFEL:
		case ARCH_TYPE_BPFEB:
		case ARCH_TYPE_SPARCEL:
		case ARCH_TYPE_LE64:
		case ARCH_TYPE_AMDIL:
		case ARCH_TYPE_AMDIL64:
		case ARCH_TYPE_HSAIL:
		case ARCH_TYPE_HSAIL64:
		case ARCH_TYPE_KALIMBA:
		case ARCH_TYPE_SHAVE:
		case ARCH_TYPE_RSCRIPT32:
		case ARCH_TYPE_RSCRIPT64:
		case ARCH_TYPE_LE32:
		case ARCH_TYPE_MIPS:
		case ARCH_TYPE_MIPSEL:
		case ARCH_TYPE_MIPS64EL:
		case ARCH_TYPE_MIPS64:
		case ARCH_TYPE_AVR:
		case ARCH_TYPE_NVPTX64:
		case ARCH_TYPE_NVPTX:
		case ARCH_TYPE_MSP430:
		case ARCH_TYPE_SYSTEMZ:
		case ARCH_TYPE_TCELE:
		case ARCH_TYPE_TCE:
		case ARCH_TYPE_LANAI:
		case ARCH_TYPE_HEXAGON:
		case ARCH_TYPE_AMDGCN:
		case ARCH_TYPE_R600:
		case ARCH_TYPE_SPARC:
		case ARCH_TYPE_SPARCV9:
		case ARCH_TYPE_XCORE:
		case ARCH_TYPE_ARC:
		case ARCH_TYPE_SPIR64:
		case ARCH_TYPE_SPIR:
			FATAL_ERROR("Unsupported arch %s.", build_target.arch_name);
			break;
		case ARCH_TYPE_AARCH64:
		case ARCH_TYPE_AARCH64_BE:
			build_target.aarch.is_darwin = os_is_apple(build_target.os);
			build_target.aarch.is_win32 = build_target.os == OS_TYPE_WIN32;
			build_target.abi = ABI_AARCH64;
			break;
		case ARCH_TYPE_WASM32:
		case ARCH_TYPE_WASM64:
			build_target.abi = ABI_WASM;
			break;
		case ARCH_TYPE_ARMB:
		case ARCH_TYPE_ARM:
		case ARCH_TYPE_THUMBEB:
		case ARCH_TYPE_THUMB:
			target_setup_arm_abi();
			break;
		case ARCH_TYPE_PPC:
			build_target.abi = ABI_PPC32;
			build_target.ppc.is_softfp = /** has spe || **/  build_target.float_abi == FLOAT_ABI_SOFT;
			break;
		case ARCH_TYPE_PPC64:
		case ARCH_TYPE_PPC64LE:
			if (build_target.object_format != OBJ_FORMAT_ELF)
			{
				if (build_target.arch == ARCH_TYPE_PPC64LE)
				{
					FATAL_ERROR("PPC64 LE non-ELF not supported.");
				}
				FATAL_ERROR("PPC64 not supported");
			}
			/** Here we need to have different codegen depending on elf version :( */
			build_target.abi = ABI_PPC64_SVR4;
			build_target.ppc64.is_softfp = build_target.float_abi == FLOAT_ABI_SOFT;
			/* todo enable if elfv2 */
			build_target.ppc64.is_elfv2 = build_target.arch == ARCH_TYPE_PPC64LE;
			/* todo enable if elfv1-qpx */
			build_target.ppc64.has_qpx = false;
			break;
		case ARCH_TYPE_RISCV64:
		case ARCH_TYPE_RISCV32:
			build_target.riscv.xlen = 0; // pointer width
			build_target.riscv.abiflen = 32; // ends with f / d (64)
			build_target.abi = ABI_RISCV;
			TODO
		case ARCH_TYPE_X86:
			target_setup_x86_abi();
			break;
		case ARCH_TYPE_X86_64:
			target_setup_x64_abi();
			build_target.x64.avx_level = 0; /* TODO */
			if (build_target.os == OS_TYPE_WIN32)
			{
				build_target.abi = ABI_WIN64;
				break;
			}
			build_target.abi = ABI_X64;
			break;
		case ARCH_TYPE_UNKNOWN:
			build_target.abi = ABI_UNKNOWN;
			break;
	}
	// TODO remove
	builtin_setup(&build_target);
}

void target_destroy()
{
	assert(build_target.machine);
	LLVMDisposeTargetMachine(build_target.machine);
}

void *target_target()
{
	return build_target.target;
}

void *target_machine()
{
	return build_target.machine;
}
void *target_data_layout()
{
	return build_target.llvm_data_layout;
}

static bool arch_is_supported(ArchType arch)
{
	switch (arch)
	{
		case ARCH_TYPE_X86_64:
			return true;
		default:
			return false;
	}
}
static ArchType arch_from_llvm_string(const char *string)
{
#define STRCASE(_str, _arch) if (strcmp(string, _str) == 0) return _arch;
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

static EnvironmentType environment_type_from_llvm_string(const char *string)
{
#define STRCASE(_str, _arch) if (strcmp(string, _str) == 0) return _arch;
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

static OsType os_from_llvm_string(const char *string)
{
#define STRCASE(_str, _os) if (strcmp(string, _str) == 0) return _os;
	STRCASE("ananas", OS_TYPE_ANANAS)
	STRCASE("cloudabi", OS_TYPE_CLOUD_ABI)
	STRCASE("darwin", OS_TYPE_DARWIN)
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
	return OS_TYPE_UNKNOWN;
#undef STRCASE
}

static VendorType vendor_from_llvm_string(const char *string)
{
#define STRCASE(_str, _vendor) if (strcmp(string, _str) == 0) return _vendor;
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
			return 0;
		case ARCH_TYPE_MSP430:
		case ARCH_TYPE_AVR:
			return 16;
		case ARCH_TYPE_ARM:
		case ARCH_TYPE_ARMB:
		case ARCH_TYPE_ARC:
		case ARCH_TYPE_HEXAGON:
		case ARCH_TYPE_MIPS:
		case ARCH_TYPE_MIPSEL:
		case ARCH_TYPE_PPC:
		case ARCH_TYPE_R600:
		case ARCH_TYPE_RISCV32:
		case ARCH_TYPE_SPARC:
		case ARCH_TYPE_SPARCEL:
		case ARCH_TYPE_TCE:
		case ARCH_TYPE_TCELE:
		case ARCH_TYPE_THUMB:
		case ARCH_TYPE_THUMBEB:
		case ARCH_TYPE_X86:
		case ARCH_TYPE_XCORE:
		case ARCH_TYPE_NVPTX:
		case ARCH_TYPE_LE32:
		case ARCH_TYPE_AMDIL:
		case ARCH_TYPE_HSAIL:
		case ARCH_TYPE_SPIR:
		case ARCH_TYPE_KALIMBA:
		case ARCH_TYPE_SHAVE:
		case ARCH_TYPE_LANAI:
		case ARCH_TYPE_WASM32:
		case ARCH_TYPE_RSCRIPT32:
		case ARCH_TYPE_AARCH64_32:
			return 32;
		case ARCH_TYPE_SPIR64:
		case ARCH_TYPE_RSCRIPT64:
		case ARCH_TYPE_WASM64:
		case ARCH_TYPE_LE64:
		case ARCH_TYPE_BPFEL:
		case ARCH_TYPE_BPFEB:
		case ARCH_TYPE_AARCH64:
		case ARCH_TYPE_AARCH64_BE:
		case ARCH_TYPE_SYSTEMZ:
		case ARCH_TYPE_SPARCV9:
		case ARCH_TYPE_MIPS64:
		case ARCH_TYPE_NVPTX64:
		case ARCH_TYPE_AMDIL64:
		case ARCH_TYPE_HSAIL64:
		case ARCH_TYPE_RISCV64:
		case ARCH_TYPE_AMDGCN:
		case ARCH_TYPE_MIPS64EL:
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

unsigned arch_is_big_endian(ArchType arch)
{
	switch (arch)
	{
		case ARCH_TYPE_UNKNOWN:
		case ARCH_TYPE_AVR:
		case ARCH_TYPE_ARC:
		case ARCH_TYPE_TCE:
		case ARCH_TYPE_TCELE:
		case ARCH_TYPE_LE32:
		case ARCH_TYPE_LE64:
		case ARCH_TYPE_AMDIL:
		case ARCH_TYPE_AMDIL64:
		case ARCH_TYPE_HSAIL:
		case ARCH_TYPE_HSAIL64:
		case ARCH_TYPE_SPIR:
		case ARCH_TYPE_SPIR64:
		case ARCH_TYPE_KALIMBA:
		case ARCH_TYPE_SHAVE:
		case ARCH_TYPE_RSCRIPT32:
		case ARCH_TYPE_RSCRIPT64:
			UNREACHABLE
		case ARCH_TYPE_X86:
		case ARCH_TYPE_X86_64:
		case ARCH_TYPE_AARCH64:
		case ARCH_TYPE_AARCH64_32:
		case ARCH_TYPE_ARM:
		case ARCH_TYPE_BPFEL:
		case ARCH_TYPE_HEXAGON:
		case ARCH_TYPE_MIPSEL:
		case ARCH_TYPE_MIPS64EL:
		case ARCH_TYPE_MSP430:
		case ARCH_TYPE_PPC64LE:
		case ARCH_TYPE_R600:
		case ARCH_TYPE_AMDGCN:
		case ARCH_TYPE_RISCV32:
		case ARCH_TYPE_RISCV64:
		case ARCH_TYPE_SPARCEL:
		case ARCH_TYPE_THUMB:
		case ARCH_TYPE_XCORE:
		case ARCH_TYPE_NVPTX:
		case ARCH_TYPE_NVPTX64:
		case ARCH_TYPE_WASM64:
		case ARCH_TYPE_WASM32:
			return false;
		case ARCH_TYPE_AARCH64_BE:
		case ARCH_TYPE_ARMB:
		case ARCH_TYPE_BPFEB:
		case ARCH_TYPE_MIPS:
		case ARCH_TYPE_MIPS64:
		case ARCH_TYPE_PPC64:
		case ARCH_TYPE_PPC:
		case ARCH_TYPE_SPARC:
		case ARCH_TYPE_SPARCV9:
		case ARCH_TYPE_SYSTEMZ:
		case ARCH_TYPE_THUMBEB:
		case ARCH_TYPE_LANAI:
			return true;
	}
	UNREACHABLE
}
unsigned os_target_supports_float16(OsType os, ArchType arch)
{
	switch (arch)
	{
		case ARCH_TYPE_AARCH64:
			return true;
		default:
			return false;
	}
}

unsigned os_target_supports_float128(OsType os, ArchType arch)
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
			if (os == OS_TYPE_DARWIN || os == OS_TYPE_MACOSX) return true;
			return false;
		default:
			return false;
	}
}

unsigned os_target_supports_vec(OsType os, ArchType arch, int bits, bool is_int)
{
	if (bits != 64 && bits != 128) return false;
	switch (arch)
	{
		case ARCH_TYPE_AARCH64:
		case ARCH_TYPE_PPC64:
		case ARCH_TYPE_PPC:
			return true;
		case ARCH_TYPE_X86:
			if (os == OS_TYPE_DARWIN || os == OS_TYPE_MACOSX)
			{
				// 64i 128f 128i
				return bits == 128 || is_int;
			}
			return false;
		default:
			return false;
	}
}

unsigned os_target_supports_int128(OsType os, ArchType arch)
{
	switch (arch)
	{
		case ARCH_TYPE_AARCH64:
			return true;
		case ARCH_TYPE_PPC:
		default:
			return false;
	}
}

unsigned os_target_c_type_bits(OsType os, ArchType arch, CType type)
{
	switch (os)
	{
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
		case OS_TYPE_DARWIN:
		case OS_TYPE_MACOSX:
		case OS_TYPE_LINUX:
		case OS_TYPE_FREE_BSD:
		case OS_TYPE_NETBSD:
		case OS_TYPE_DRAGON_FLY:
		case OS_TYPE_OPENBSD:
		case OS_TYPE_WASI:
		case OS_TYPE_EMSCRIPTEN:
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
		case OS_TYPE_ANANAS:
		case OS_TYPE_CLOUD_ABI:
		case OS_TYPE_FUCHSIA:
		case OS_TYPE_KFREEBSD:
		case OS_TYPE_PS3:
		case OS_TYPE_SOLARIS:
		case OS_TYPE_HAIKU:
		case OS_TYPE_MINIX:
		case OS_TYPE_RTEMS:
		case OS_TYPE_NACL:
		case OS_TYPE_CNK:
		case OS_TYPE_AIX:
		case OS_TYPE_CUDA:
		case OS_TYPE_NVOPENCL:
		case OS_TYPE_AMDHSA:
		case OS_TYPE_PS4:
		case OS_TYPE_ELFIAMCU:
		case OS_TYPE_TVOS:
		case OS_TYPE_WATCHOS:
		case OS_TYPE_MESA3D:
		case OS_TYPE_CONTIKI:
		case OS_TYPE_AMDPAL:
		case OS_TYPE_HERMITCORE:
		case OS_TYPE_HURD:
			TODO
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

static unsigned os_target_alignment_of_int(OsType os, ArchType arch, int bits)
{
	switch (arch)
	{
		case ARCH_TYPE_UNKNOWN:
		case ARCH_TYPE_AVR:
		case ARCH_TYPE_ARC:
		case ARCH_TYPE_TCE:
		case ARCH_TYPE_TCELE:
		case ARCH_TYPE_LE32:
		case ARCH_TYPE_LE64:
		case ARCH_TYPE_AMDIL:
		case ARCH_TYPE_AMDIL64:
		case ARCH_TYPE_HSAIL:
		case ARCH_TYPE_HSAIL64:
		case ARCH_TYPE_SPIR:
		case ARCH_TYPE_SPIR64:
		case ARCH_TYPE_KALIMBA:
		case ARCH_TYPE_SHAVE:
		case ARCH_TYPE_RSCRIPT32:
		case ARCH_TYPE_RSCRIPT64:
			UNREACHABLE
		case ARCH_TYPE_ARM:
		case ARCH_TYPE_THUMB:
			if ((os_is_apple(os) || os == OS_TYPE_NETBSD) && bits > 32) return 4;
			return bits > 64 ? 8 : bits / 8;
		case ARCH_TYPE_ARMB:
		case ARCH_TYPE_THUMBEB:
			if (os == OS_TYPE_NETBSD && bits > 32) return 4;
			return bits > 64 ? 8 : bits / 8;
		case ARCH_TYPE_BPFEB:
		case ARCH_TYPE_BPFEL:
		case ARCH_TYPE_HEXAGON:
		case ARCH_TYPE_MIPS:
		case ARCH_TYPE_MIPSEL:
		case ARCH_TYPE_MIPS64:
		case ARCH_TYPE_MIPS64EL:
		case ARCH_TYPE_PPC64:
		case ARCH_TYPE_PPC:
		case ARCH_TYPE_PPC64LE:
		case ARCH_TYPE_R600:
		case ARCH_TYPE_AMDGCN:
		case ARCH_TYPE_RISCV32:
		case ARCH_TYPE_SPARC:
		case ARCH_TYPE_SPARCEL:
		case ARCH_TYPE_SPARCV9:
		case ARCH_TYPE_SYSTEMZ:
		case ARCH_TYPE_X86_64:
		case ARCH_TYPE_LANAI:
		case ARCH_TYPE_WASM32:
		case ARCH_TYPE_WASM64:
			return bits > 64 ? 8 : bits / 8;
		case ARCH_TYPE_XCORE:
			return bits > 32 ? 4 : bits / 8;
		case ARCH_TYPE_MSP430:
			return bits > 16 ? 2 : bits / 8;
		case ARCH_TYPE_AARCH64:
		case ARCH_TYPE_AARCH64_32:
		case ARCH_TYPE_AARCH64_BE:
		case ARCH_TYPE_RISCV64:
		case ARCH_TYPE_NVPTX:
		case ARCH_TYPE_NVPTX64:
			return bits / 8;
		case ARCH_TYPE_X86:
			if (bits >= 64)
			{
				return (os == OS_TYPE_WIN32 || os == OS_TYPE_NACL) ? 8 : 4;
			}
			return bits / 8;
	}
	UNREACHABLE
}

static unsigned os_target_pref_alignment_of_int(OsType os, ArchType arch, int bits)
{
	switch (arch)
	{
		case ARCH_TYPE_UNKNOWN:
		case ARCH_TYPE_AVR:
		case ARCH_TYPE_ARC:
		case ARCH_TYPE_TCE:
		case ARCH_TYPE_TCELE:
		case ARCH_TYPE_LE32:
		case ARCH_TYPE_LE64:
		case ARCH_TYPE_AMDIL:
		case ARCH_TYPE_AMDIL64:
		case ARCH_TYPE_HSAIL:
		case ARCH_TYPE_HSAIL64:
		case ARCH_TYPE_SPIR:
		case ARCH_TYPE_SPIR64:
		case ARCH_TYPE_KALIMBA:
		case ARCH_TYPE_SHAVE:
		case ARCH_TYPE_RSCRIPT32:
		case ARCH_TYPE_RSCRIPT64:
			UNREACHABLE
		case ARCH_TYPE_X86:
			if (os == OS_TYPE_ELFIAMCU && bits > 32) return 4;
			return bits > 64 ? 8 : bits / 8;
		case ARCH_TYPE_AARCH64:
		case ARCH_TYPE_AARCH64_32:
			if (bits < 32 && !os_is_apple(os) && os != OS_TYPE_WIN32) return 4;
			return bits / 8;
		case ARCH_TYPE_AARCH64_BE:
			return bits < 32 ? 4 : bits / 8;
		case ARCH_TYPE_ARM:
		case ARCH_TYPE_ARMB:
		case ARCH_TYPE_BPFEB:
		case ARCH_TYPE_BPFEL:
		case ARCH_TYPE_HEXAGON:
		case ARCH_TYPE_PPC:
		case ARCH_TYPE_PPC64LE:
		case ARCH_TYPE_PPC64:
		case ARCH_TYPE_R600:
		case ARCH_TYPE_AMDGCN:
		case ARCH_TYPE_RISCV32:
		case ARCH_TYPE_SPARC:
		case ARCH_TYPE_SPARCEL:
		case ARCH_TYPE_SPARCV9:
		case ARCH_TYPE_THUMB:
		case ARCH_TYPE_THUMBEB:
		case ARCH_TYPE_X86_64:
		case ARCH_TYPE_LANAI:
		case ARCH_TYPE_WASM32:
		case ARCH_TYPE_WASM64:
			return bits < 64 ? bits / 8 : 8;
		case ARCH_TYPE_XCORE:
			return 4;
		case ARCH_TYPE_SYSTEMZ:
			if (bits <= 16) return 2;
			return bits < 64 ? bits / 8 : 8;
		case ARCH_TYPE_MIPS:
		case ARCH_TYPE_MIPSEL:
		case ARCH_TYPE_MIPS64EL:
		case ARCH_TYPE_MIPS64:
			if (bits < 32) return 4;
			return bits < 64 ? bits / 8 : 8;
		case ARCH_TYPE_MSP430:
			return bits < 16 ? bits / 8 : 2;
		case ARCH_TYPE_RISCV64:
		case ARCH_TYPE_NVPTX:
		case ARCH_TYPE_NVPTX64:
			return bits / 8;
	}
	UNREACHABLE
}

static unsigned os_target_alignment_of_float(OsType os, ArchType arch, int bits)
{
	switch (arch)
	{
		case ARCH_TYPE_UNKNOWN:
		case ARCH_TYPE_AVR:
		case ARCH_TYPE_ARC:
		case ARCH_TYPE_TCE:
		case ARCH_TYPE_TCELE:
		case ARCH_TYPE_LE32:
		case ARCH_TYPE_LE64:
		case ARCH_TYPE_AMDIL:
		case ARCH_TYPE_AMDIL64:
		case ARCH_TYPE_HSAIL:
		case ARCH_TYPE_HSAIL64:
		case ARCH_TYPE_SPIR:
		case ARCH_TYPE_SPIR64:
		case ARCH_TYPE_KALIMBA:
		case ARCH_TYPE_SHAVE:
		case ARCH_TYPE_RSCRIPT32:
		case ARCH_TYPE_RSCRIPT64:
			UNREACHABLE
		case ARCH_TYPE_X86:
			if (os == OS_TYPE_ELFIAMCU && bits >= 32) return 4;
			if (os == OS_TYPE_WIN32 || os == OS_TYPE_NACL)
			{
				return bits / 8;
			}
			return bits == 64 ? 4 : bits / 8;
		case ARCH_TYPE_AARCH64:
		case ARCH_TYPE_AARCH64_BE:
		case ARCH_TYPE_AARCH64_32:
		case ARCH_TYPE_BPFEB:
		case ARCH_TYPE_BPFEL:
		case ARCH_TYPE_HEXAGON:
		case ARCH_TYPE_MIPS:
		case ARCH_TYPE_MIPSEL:
		case ARCH_TYPE_MIPS64:
		case ARCH_TYPE_MIPS64EL:
		case ARCH_TYPE_PPC64:
		case ARCH_TYPE_PPC64LE:
		case ARCH_TYPE_PPC:
		case ARCH_TYPE_R600:
		case ARCH_TYPE_AMDGCN:
		case ARCH_TYPE_RISCV32:
		case ARCH_TYPE_RISCV64:
		case ARCH_TYPE_SPARCV9:
		case ARCH_TYPE_NVPTX:
		case ARCH_TYPE_NVPTX64:
		case ARCH_TYPE_LANAI:
		case ARCH_TYPE_WASM32:
		case ARCH_TYPE_WASM64:
			return bits / 8;
		case ARCH_TYPE_MSP430:
			return bits < 128 ? 2 : 16;
		case ARCH_TYPE_ARM:
		case ARCH_TYPE_THUMB:
			if ((os_is_apple(os) || os == OS_TYPE_NETBSD) && bits == 64)
			{
				return 4;
			}
			return bits / 8;
		case ARCH_TYPE_THUMBEB:
		case ARCH_TYPE_ARMB:
			if (os == OS_TYPE_NETBSD && bits == 64)
			{
				return 4;
			}
			return bits / 8;
		case ARCH_TYPE_SPARC:
		case ARCH_TYPE_SPARCEL:
		case ARCH_TYPE_SYSTEMZ:
			return bits < 64 ? bits / 8 : 8;
		case ARCH_TYPE_X86_64:
			if (bits == 128 && os == OS_TYPE_ELFIAMCU) return 4;
			return bits / 8;
		case ARCH_TYPE_XCORE:
			if (bits == 64) return 4;
			return bits / 8;
	}
	UNREACHABLE
}

static unsigned os_target_pref_alignment_of_float(OsType os, ArchType arch, int bits)
{
	switch (arch)
	{
		case ARCH_TYPE_UNKNOWN:
		case ARCH_TYPE_AVR:
		case ARCH_TYPE_ARC:
		case ARCH_TYPE_TCE:
		case ARCH_TYPE_TCELE:
		case ARCH_TYPE_LE32:
		case ARCH_TYPE_LE64:
		case ARCH_TYPE_AMDIL:
		case ARCH_TYPE_AMDIL64:
		case ARCH_TYPE_HSAIL:
		case ARCH_TYPE_HSAIL64:
		case ARCH_TYPE_SPIR:
		case ARCH_TYPE_SPIR64:
		case ARCH_TYPE_KALIMBA:
		case ARCH_TYPE_SHAVE:
		case ARCH_TYPE_RSCRIPT32:
		case ARCH_TYPE_RSCRIPT64:
			UNREACHABLE
		case ARCH_TYPE_X86:
			if (os == OS_TYPE_ELFIAMCU && bits >= 32) return 4;
			return bits / 8;
		case ARCH_TYPE_AARCH64:
		case ARCH_TYPE_AARCH64_BE:
		case ARCH_TYPE_AARCH64_32:
		case ARCH_TYPE_BPFEB:
		case ARCH_TYPE_BPFEL:
		case ARCH_TYPE_HEXAGON:
		case ARCH_TYPE_MIPS:
		case ARCH_TYPE_MIPSEL:
		case ARCH_TYPE_MIPS64:
		case ARCH_TYPE_MIPS64EL:
		case ARCH_TYPE_PPC64:
		case ARCH_TYPE_PPC64LE:
		case ARCH_TYPE_PPC:
		case ARCH_TYPE_R600:
		case ARCH_TYPE_AMDGCN:
		case ARCH_TYPE_RISCV32:
		case ARCH_TYPE_RISCV64:
		case ARCH_TYPE_SPARCV9:
		case ARCH_TYPE_NVPTX:
		case ARCH_TYPE_NVPTX64:
		case ARCH_TYPE_LANAI:
		case ARCH_TYPE_WASM32:
		case ARCH_TYPE_WASM64:
		case ARCH_TYPE_ARM:
		case ARCH_TYPE_THUMB:
		case ARCH_TYPE_THUMBEB:
		case ARCH_TYPE_ARMB:
			return bits / 8;
		case ARCH_TYPE_MSP430:
			return bits < 128 ? 2 : 16;
		case ARCH_TYPE_SPARC:
		case ARCH_TYPE_SPARCEL:
		case ARCH_TYPE_SYSTEMZ:
			return bits < 64 ? bits / 8 : 8;
		case ARCH_TYPE_X86_64:
			if (bits == 128 && os == OS_TYPE_ELFIAMCU) return 4;
			return bits / 8;
		case ARCH_TYPE_XCORE:
			if (bits == 64) return 4;
			return bits / 8;
	}
	UNREACHABLE
}

