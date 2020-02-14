#include <llvm-c/Target.h>
#include <llvm-c/TargetMachine.h>
#include <llvm-c/Core.h>
#include <target_info/target_info.h>
#include "compiler_internal.h"

static unsigned arch_pointer_bit_width(ArchType arch);
static ArchType arch_from_llvm_string(const char *string);
static unsigned os_target_c_type_bits(OsType os, ArchType arch, CType type);
static OsType os_from_llvm_string(const char *string);
static VendorType vendor_from_llvm_string(const char *string);

Target build_target = {};

int target_alloca_addr_space()
{
	return build_target.alloca_address_space;
}


void target_setup()
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
	build_target.os_name = strdup(strtok(NULL, "0123456789"));

	LLVMDisposeMessage(target_triple);

	build_target.arch = arch_from_llvm_string(build_target.arch_name);
	build_target.os = os_from_llvm_string(build_target.os_name);
	build_target.vendor = vendor_from_llvm_string(build_target.vendor_name);

	build_target.width_pointer = arch_pointer_bit_width(build_target.arch);
	assert(build_target.width_pointer == LLVMPointerSize(build_target.llvm_data_layout) * 8);
	build_target.alloca_address_space = 0;


	LLVMTypeRef byte_type = LLVMIntType(8);
	LLVMTypeRef short_type = LLVMIntType(16);
	LLVMTypeRef int_type = LLVMIntType(32);
	LLVMTypeRef long_type = LLVMIntType(64);
	LLVMTypeRef float_type = LLVMFloatType();
	LLVMTypeRef double_type = LLVMDoubleType();
	LLVMTypeRef quad_type = LLVMFP128Type();
	build_target.align_byte = LLVMABIAlignmentOfType(build_target.llvm_data_layout, byte_type);
	build_target.align_short = LLVMABIAlignmentOfType(build_target.llvm_data_layout, short_type);
	build_target.align_int = LLVMABIAlignmentOfType(build_target.llvm_data_layout, int_type);
	build_target.align_long = LLVMABIAlignmentOfType(build_target.llvm_data_layout, long_type);
	build_target.align_f128 = LLVMABIAlignmentOfType(build_target.llvm_data_layout, quad_type);
	build_target.align_double = LLVMABIAlignmentOfType(build_target.llvm_data_layout, double_type);
	build_target.align_float = LLVMABIAlignmentOfType(build_target.llvm_data_layout, float_type);
	build_target.little_endian = LLVMByteOrder(build_target.llvm_data_layout) == LLVMLittleEndian;
	build_target.width_c_short = os_target_c_type_bits(build_target.os, build_target.arch, CTYPE_SHORT);
	build_target.width_c_int = os_target_c_type_bits(build_target.os, build_target.arch, CTYPE_INT);
	build_target.width_c_long = os_target_c_type_bits(build_target.os, build_target.arch, CTYPE_LONG);
	build_target.width_c_long_long = os_target_c_type_bits(build_target.os, build_target.arch, CTYPE_LONG_LONG);

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

static unsigned arch_pointer_bit_width(ArchType arch)
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
		case ARCH_TYPE_AARCH64_32:
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
			return 32;
		case ARCH_TYPE_SPIR64:
		case ARCH_TYPE_RSCRIPT64:
		case ARCH_TYPE_WASM64:
		case ARCH_TYPE_LE64:
		case ARCH_TYPE_BPFEL:
		case ARCH_TYPE_BPFEB:
		case ARCH_TYPE_AARCH64:
		case ARCH_TYPE_AARCH64_BE:
		case ARCH_TYPE_X86_64:
		case ARCH_TYPE_SYSTEMZ:
		case ARCH_TYPE_PPC64:
		case ARCH_TYPE_SPARCV9:
		case ARCH_TYPE_MIPS64:
		case ARCH_TYPE_NVPTX64:
		case ARCH_TYPE_AMDIL64:
		case ARCH_TYPE_HSAIL64:
		case ARCH_TYPE_RISCV64:
		case ARCH_TYPE_AMDGCN:
		case ARCH_TYPE_MIPS64EL:
		case ARCH_TYPE_PPC64LE:
			return 64;
		default:
			UNREACHABLE
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
		case OS_TYPE_LINUX:
		case OS_TYPE_DARWIN:
		case OS_TYPE_MACOSX:
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
			return arch_pointer_bit_width(arch);
		case CTYPE_LONG_LONG:
			return 64;
		default:
			UNREACHABLE
	}

}
