#if LLVM_AVAILABLE
#include <llvm-c/Target.h>
#include <llvm-c/TargetMachine.h>
#include <llvm-c/Core.h>
#endif
#include "compiler_internal.h"
#if LLVM_AVAILABLE
#include "c3_llvm.h"
#else
#include "utils/hostinfo.h"
#endif

static bool cpu_features_contains(CpuFeatures *cpu_features, int feature);
static ObjectFormatType object_format_from_os(OsType os, ArchType arch_type);
static unsigned arch_pointer_bit_width(OsType os, ArchType arch);
static unsigned arch_int_register_bit_width(OsType os, ArchType arch);
static ArchType arch_from_llvm_string(StringSlice string);
static EnvironmentType environment_type_from_llvm_string(StringSlice string);
static bool arch_is_supported(ArchType arch);
static unsigned os_target_c_type_bits(OsType os, ArchType arch, CType type);
static AlignData os_target_alignment_of_int(OsType os, ArchType arch, uint32_t bits);
static AlignData os_target_alignment_of_float(OsType os, ArchType arch, uint32_t bits);
static OsType os_from_llvm_string(StringSlice string);
static VendorType vendor_from_llvm_string(StringSlice string);
static unsigned os_target_supports_int128(OsType os, ArchType arch);
static unsigned os_target_supports_float16(OsType os, ArchType arch);
static unsigned os_target_supports_float128(OsType os, ArchType arch);
static unsigned os_target_supports_vec(OsType os, ArchType arch, int bits, bool is_int);
static void x86_features_from_host(CpuFeatures *cpu_features);
static void x86_features_add_feature(CpuFeatures *cpu_features, X86Feature feature);
static void cpu_features_add_feature_single(CpuFeatures *cpu_features, int feature);
static void cpu_features_set_to_features(CpuFeatures enabled_features, CpuFeatures cpu_defaults, const bool *skip, const char *list[], int last);
static void update_cpu_features(const char *features, CpuFeatures *cpu_features, const char **feature_names, int feature_count);

const CpuFeatures cpu_feature_zero = { { 0, 0} };

static const char *wasm_feature_name[] = {
	[WASM_FEAT_BULK_MEMORY] = "bulk-memory",
	[WASM_FEAT_BULK_MEMORY_OPT] = "bulk-memory-opt",
	[WASM_FEAT_CALL_INDIRECT_OVERLONG] = "call-indirect-overlong",
	[WASM_FEAT_MULTIVALUE] = "multivalue",
	[WASM_FEAT_MUTABLE_GLOBALS] = "mutable-globals",
	[WASM_FEAT_NONTRAPPING_FPTORINT] = "nontrapping-fptoint",
	[WASM_FEAT_REFERENCE_TYPES] = "reference-types",
	[WASM_FEAT_SIGN_EXT] = "sign-ext"
};

static const char *arm_feature_name[] = {
	[ARM_FEAT_ARMV6KZ] = "armv6kz",
	[ARM_FEAT_DSP] = "dsp",
	[ARM_FEAT_FP64] = "fp64",
	[ARM_FEAT_STRICT_ALIGN] = "strict-align",
	[ARM_FEAT_VFP2] = "vfp2",
	[ARM_FEAT_VFP2SP] = "vfp2sp",
	[ARM_FEAT_AES] = "aes",
	[ARM_FEAT_D32] = "d32",
	[ARM_FEAT_FP_ARMV8] = "fp-armv8",
	[ARM_FEAT_FP_ARMV8D16] = "fp-armv8d16",
	[ARM_FEAT_FP_ARMV8D16SP] = "fp-armv8d16sp",
	[ARM_FEAT_FP_ARMV8SP] = "fp-armv8sp",
	[ARM_FEAT_FP_FP16] = "fp16",
	[ARM_FEAT_FP_FP16FML] = "fp16fml",
	[ARM_FEAT_FP_FULLFP16] = "fullfp16",
	[ARM_FEAT_NEON] = "neon",
	[ARM_FEAT_SHA2] = "sha2",
	[ARM_FEAT_THUMB_MODE] = "thumb-mode",
	[ARM_FEAT_VFP3] = "vfp3",
	[ARM_FEAT_VFP3D16] = "vfp3d16",
	[ARM_FEAT_VFP3D16SP] = "vfp3d16sp",
	[ARM_FEAT_VFP3SP] = "vfp3sp"
};

static const char *aarch64_feature_name[] = {
	[AARCH64_FEAT_CRC] = "crc",
	[AARCH64_FEAT_LSE] = "lse",
	[AARCH64_FEAT_RDM] = "rdm",
	[AARCH64_FEAT_FP_ARMV8] = "fp-armv8",
	[AARCH64_FEAT_NEON] = "neon"
};

static const char *riscv_feature_name[] = {
	[RISCV_FEAT_64BIT] = "64bit",
	[RISCV_FEAT_32BIT] = "32bit",
	[RISCV_FEAT_A] = "a",
	[RISCV_FEAT_B] = "b",
	[RISCV_FEAT_C] = "c",
	[RISCV_FEAT_D] = "d",
	[RISCV_FEAT_E] = "e",
	[RISCV_FEAT_F] = "f",
	[RISCV_FEAT_I] = "i",
	[RISCV_FEAT_M] = "m",
	[RISCV_FEAT_RELAX] = "relax",
	[RISCV_FEAT_V] = "v",
	[RISCV_FEAT_ZAAMO] = "zaamo",
	[RISCV_FEAT_ZALRSC] = "zalrsc",
	[RISCV_FEAT_ZBA] = "zba",
	[RISCV_FEAT_ZBB] = "zbb",
	[RISCV_FEAT_ZBS] = "zbs",
	[RISCV_FEAT_ZCA] = "zca",
	[RISCV_FEAT_ZCD] = "zcd",
	[RISCV_FEAT_ZCF] = "zcf",
	[RISCV_FEAT_ZFA] = "zfa",
	[RISCV_FEAT_ZICSR] = "zicsr",
	[RISCV_FEAT_ZIFENCEI] = "zifencei",
	[RISCV_FEAT_ZMMUL] = "zmmul",
};

static const char *x86_feature_name[] = {
	[X86_FEAT_CMOV] = "cmov",
	[X86_FEAT_MMX] = "mmx",
	[X86_FEAT_POPCNT] = "popcnt",
	[X86_FEAT_SSE] = "sse",
	[X86_FEAT_SSE2] = "sse2",
	[X86_FEAT_SSE3] = "sse3",
	[X86_FEAT_SSSE3] = "ssse3",
	[X86_FEAT_SSE4_1] = "sse4.1",
	[X86_FEAT_SSE4_2] = "sse4.2",
	[X86_FEAT_APX_EGPR] = "egpr",
	[X86_FEAT_APX_PUSH2POP2] = "push2pop2",
	[X86_FEAT_APX_PPX] = "ppx",
	[X86_FEAT_APX_NDD] = "ndd",
	[X86_FEAT_APX_CCMP] = "ccmp",
	[X86_FEAT_APX_NF] = "nf",
	[X86_FEAT_APX_CF] = "cf",
	[X86_FEAT_APX_ZU] = "zu",
	[X86_FEAT_AVX] = "avx",
	[X86_FEAT_AVX2] = "avx2",
	[X86_FEAT_AVX10_1_512] = "avx10.1-512",
	[X86_FEAT_AVX10_1_256] = "avx10.1-256",
	[X86_FEAT_AVX10_2_512] = "avx10.2-512",
	[X86_FEAT_AVX10_2_256] = "avx10.2-256",
	[X86_FEAT_SSE4_A] = "sse4a",
	[X86_FEAT_FMA4] = "fma4",
	[X86_FEAT_XOP] = "xop",
	[X86_FEAT_FMA] = "fma",
	[X86_FEAT_AVX512F] = "avx512f",
	[X86_FEAT_BMI] = "bmi",
	[X86_FEAT_BMI2] = "bmi2",
	[X86_FEAT_AES] = "aes",
	[X86_FEAT_PCLMUL] = "pclmul",
	[X86_FEAT_AVX512VL] = "avx512vl",
	[X86_FEAT_AVX512BW] = "avx512bw",
	[X86_FEAT_AVX512DQ] = "avx512dq",
	[X86_FEAT_AVX512CD] = "avx512cd",
	[X86_FEAT_AVX512ER] = "avx512er",
	[X86_FEAT_AVX512PF] = "avx512pf",
	[X86_FEAT_AVX512VBMI] = "avx512vbmi",
	[X86_FEAT_AVX512IFMA] = "avx512ifma",
	[X86_FEAT_AVX5124VNNIW] = "avx5124vnniw",
	[X86_FEAT_AVX5124FMAPS] = "avx5124fmaps",
	[X86_FEAT_AVX512VPOPCNTDQ] = "avx512vpopcntdq",
	[X86_FEAT_AVX512VBMI2] = "avx512vbmi2",
	[X86_FEAT_GFNI] = "gfni",
	[X86_FEAT_VPCLMULQDQ] = "vpclmulqdq",
	[X86_FEAT_AVX512VNNI] = "avx512vnni",
	[X86_FEAT_AVX512BITALG] = "avx512bitalg",
	[X86_FEAT_AVX512BF16] = "avx512bf16",
	[X86_FEAT_AVX512VP2INTERSECT] = "avx512vp2intersect",
	[X86_FEAT_ADX] = "adx",
	[X86_FEAT_AMX_BF16] = "amx-bf16",
	[X86_FEAT_AMX_INT8] = "amx-int8",
	[X86_FEAT_AMX_TILE] = "amx-tile",
	[X86_FEAT_AMX_AVX512] = "amx-avx512",
	[X86_FEAT_AMX_FP8] = "amx-fp8",
	[X86_FEAT_AMX_MOVRS] = "amx-movrs",
	[X86_FEAT_AMX_TF32] = "amx-tf32",
	[X86_FEAT_AMX_TRANSPOSE] = "amx-transpose",
	[X86_FEAT_MOVRS] = "movrs",
	[X86_FEAT_CLDEMOTE] = "cldemote",
	[X86_FEAT_CLFLUSHOPT] = "clflushopt",
	[X86_FEAT_CLWB] = "clwb",
	[X86_FEAT_CLZERO] = "clzero",
	[X86_FEAT_CMPXCHG16B] = "cx16",
	[X86_FEAT_CMPXCHG8B] = "cx8",
	[X86_FEAT_CRC32] = "crc32",
	[X86_FEAT_ENQCMD] = "enqcmd",
	[X86_FEAT_F16C] = "f16c",
	[X86_FEAT_FSGSBASE] = "fsgsbase",
	[X86_FEAT_FXSR] = "fxsr",
	[X86_FEAT_INVPCID] = "invpcid",
	[X86_FEAT_KL] = "kl",
	[X86_FEAT_WIDEKL] = "widekl",
	[X86_FEAT_LWP] = "lwp",
	[X86_FEAT_LZCNT] = "lzcnt",
	[X86_FEAT_MOVBE] = "movbe",
	[X86_FEAT_MOVDIR64B] = "movdir64b",
	[X86_FEAT_MOVDIRI] = "movdiri",
	[X86_FEAT_MWAITX] = "mwaitx",
	[X86_FEAT_PCONFIG] = "pconfig",
	[X86_FEAT_PKU] = "pku",
	[X86_FEAT_PREFETCHI] = "prefetchi",
	[X86_FEAT_PREFETCHWT1] = "prefetchwt1",
	[X86_FEAT_PRFCHW] = "prfchw",
	[X86_FEAT_PTWRITE] = "ptwrite",
	[X86_FEAT_RDPID] = "rdpid",
	[X86_FEAT_RDPRU] = "rdpru",
	[X86_FEAT_RDRND] = "rdrnd",
	[X86_FEAT_RDSEED] = "rdseed",
	[X86_FEAT_RTM] = "rtm",
	[X86_FEAT_SAHF] = "sahf",
	[X86_FEAT_SERIALIZE] = "serialize",
	[X86_FEAT_SGX] = "sgx",
	[X86_FEAT_SM3] = "sm3",
	[X86_FEAT_SM4] = "sm4",
	[X86_FEAT_SHA] = "sha",
	[X86_FEAT_SHA512] = "sha512",
	[X86_FEAT_SHSTK] = "shstk",
	[X86_FEAT_TBM] = "tbm",
	[X86_FEAT_TSXLDTRK] = "tsxldtrk",
	[X86_FEAT_UINTR] = "uintr",
	[X86_FEAT_USERMSR] = "usermsr",
	[X86_FEAT_VAES] = "vaes",
	[X86_FEAT_VZEROUPPER] = "vzeroupper",
	[X86_FEAT_WAITPKG] = "waitpkg",
	[X86_FEAT_WBNOINVD] = "wbnoinvd",
	[X86_FEAT_X87] = "x87",
	[X86_FEAT_XSAVE] = "xsave",
	[X86_FEAT_XSAVEC] = "xsavec",
	[X86_FEAT_XSAVEOPT] = "xsaveopt",
	[X86_FEAT_XSAVES] = "xsaves",
	[X86_FEAT_HRESET] = "hreset",
	[X86_FEAT_RAOINT] = "raoint",
	[X86_FEAT_AVX512FP16] = "avx512fp16",
	[X86_FEAT_AMX_FP16] = "amx-fp16",
	[X86_FEAT_AMX_COMPLEX] = "amx-complex",
	[X86_FEAT_CMPCCXADD] = "cmpccxadd",
	[X86_FEAT_AVXNECONVERT] = "avxneconvert",
	[X86_FEAT_AVXVNNI] = "avxvnni",
	[X86_FEAT_AVXIFMA] = "avxifma",
	[X86_FEAT_AVXVNNIINT8] = "avxvnniint8",
	[X86_FEAT_AVXVNNIINT16] = "avxvnniint16",
	[X86_FEAT_EVEX512] = "evex512",
	[X86_FEAT_SOFT_FLOAT] = "soft-float",
};

int target_alloca_addr_space()
{
	return compiler.platform.alloca_address_space;
}

bool os_supports_stacktrace(OsType os_type)
{
	return os_type == OS_TYPE_LINUX || os_is_apple(os_type) || os_type == OS_TYPE_WIN32;
}
bool os_is_apple(OsType os_type)
{
	return os_type == OS_TYPE_TVOS || os_type == OS_TYPE_WATCHOS ||
		   os_type == OS_TYPE_MACOSX || os_type == OS_TYPE_IOS;
}

bool arch_is_wasm(ArchType type)
{
	return type == ARCH_TYPE_WASM32 || type == ARCH_TYPE_WASM64;
}

static AlignSize os_arch_max_alignment_of_vector(OsType os, ArchType arch, EnvironmentType type, ARMVariant variant, CpuFeatures* x64_features)
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
				return 8192 / 8;
			}
			if (os_is_apple(os))
			{
				// With AVX512 - 512, AVX - 256 otherwise AVX - 128
				if (cpu_features_contains(x64_features, X86_FEAT_AVX512F)) return 512 / 8;
				if (cpu_features_contains(x64_features, X86_FEAT_AVX)) return 256 / 8;
				return 128 / 8;
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
		case OS_DARWIN_TYPES:
		case OS_TYPE_FREEBSD:
		case OS_TYPE_LINUX:
		case OS_TYPE_ANDROID:
		case OS_TYPE_NETBSD:
		case OS_TYPE_OPENBSD:
		case OS_TYPE_WIN32:
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

static inline void target_setup_aarch64_abi(void)
{
	compiler.platform.aarch.is_darwin = os_is_apple(compiler.platform.os);
	compiler.platform.aarch.is_win32 = compiler.platform.os == OS_TYPE_WIN32;
	compiler.platform.abi = ABI_AARCH64;
	// TODO improve Aarch64 functionality support.
	CpuFeatures features = cpu_feature_zero;
	cpu_features_add_feature_single(&features, AARCH64_FEAT_CRC);
	cpu_features_add_feature_single(&features, AARCH64_FEAT_LSE);
	cpu_features_add_feature_single(&features, AARCH64_FEAT_RDM);
	cpu_features_add_feature_single(&features, AARCH64_FEAT_FP_ARMV8);
	cpu_features_add_feature_single(&features, AARCH64_FEAT_NEON);
	update_cpu_features(compiler.build.cpu_flags, &features, aarch64_feature_name, AARCH64_FEATURE_LAST);
	cpu_features_set_to_features(features, cpu_feature_zero, NULL, aarch64_feature_name, AARCH64_FEATURE_LAST);
}

static inline void target_setup_arm_abi(void)
{
	compiler.platform.abi = ABI_ARM;
	if (compiler.platform.os)
	{
		compiler.platform.arm.is_win32 = true;
		compiler.platform.arm.variant = ARM_AAPCS;
		compiler.platform.arm.abi_variant = ARM_ABI_AAPCS16_VFP;
		return;
	}

	CpuFeatures features = cpu_feature_zero;

	// Should be based on feature set. We start with this though.
	cpu_features_add_feature_single(&features, ARM_FEAT_ARMV6KZ);
	cpu_features_add_feature_single(&features, ARM_FEAT_DSP);
	cpu_features_add_feature_single(&features, ARM_FEAT_FP64);
	cpu_features_add_feature_single(&features, ARM_FEAT_STRICT_ALIGN);
	cpu_features_add_feature_single(&features, ARM_FEAT_VFP2);
	cpu_features_add_feature_single(&features, ARM_FEAT_VFP2SP);

	update_cpu_features(compiler.build.cpu_flags, &features, arm_feature_name, ARM_FEAT_LAST);
	cpu_features_set_to_features(features, cpu_feature_zero, NULL, arm_feature_name, ARM_FEAT_LAST);
	if (compiler.platform.object_format == OBJ_FORMAT_MACHO)
	{
		if (compiler.platform.environment_type == ENV_TYPE_EABI
			|| compiler.platform.os == OS_TYPE_UNKNOWN /* or is M */)
		{
			compiler.platform.arm.variant = ARM_AAPCS;
			return;
		}
		// TODO
		if (/* is watch abi */ false)
		{
			compiler.platform.arm.variant = ARM_AAPCS16;
			goto SET_ABI;
		}
		compiler.platform.arm.variant = ARM_APCS_GNU;
		goto SET_ABI;
	}
	switch (compiler.platform.environment_type)
	{
		case ENV_TYPE_ANDROID:
		case ENV_TYPE_GNUEABI:
		case ENV_TYPE_GNUEABIHF:
		case ENV_TYPE_MUSLEABI:
		case ENV_TYPE_MUSLEABIHF:
			compiler.platform.arm.variant = ARM_AAPCS_LINUX;
			goto SET_ABI;
		case ENV_TYPE_EABI:
		case ENV_TYPE_EABIHF:
			compiler.platform.arm.variant = ARM_AAPCS;
			goto SET_ABI;
		case ENV_TYPE_GNU:
			compiler.platform.arm.variant = ARM_APCS_GNU;
			goto SET_ABI;
		default:
			break;
	}
	switch (compiler.platform.os)
	{
		case OS_TYPE_NETBSD:
			compiler.platform.arm.variant = ARM_APCS_GNU;
			break;
		case OS_TYPE_OPENBSD:
			compiler.platform.arm.variant = ARM_AAPCS_LINUX;
			break;
		default:
			compiler.platform.arm.variant = ARM_AAPCS;
			break;
	}
	SET_ABI:
	switch (compiler.platform.arm.variant)
	{
		case ARM_APCS_GNU:
			compiler.platform.arm.abi_variant = ARM_ABI_APCS;
			return;
		case ARM_AAPCS16:
			compiler.platform.arm.abi_variant = ARM_ABI_AAPCS16_VFP;
			return;
		case ARM_AAPCS:
		case ARM_AAPCS_LINUX:
			if (compiler.platform.float_abi == FLOAT_ABI_HARD ||
				(compiler.platform.float_abi != FLOAT_ABI_SOFT &&
				 (compiler.platform.environment_type == ENV_TYPE_GNUEABIHF ||
				  compiler.platform.environment_type == ENV_TYPE_MUSLEABIHF ||
				  compiler.platform.environment_type == ENV_TYPE_EABIHF)))
			{
				compiler.platform.arm.abi_variant = ARM_ABI_AAPCS_VFP;
				return;
			}
			compiler.platform.arm.abi_variant = ARM_ABI_AAPCS;
			break;
	}
	UNREACHABLE_VOID
}

static inline void target_setup_x86_abi(BuildTarget *target)
{
	compiler.platform.abi = ABI_X86;
	compiler.platform.x86.use_soft_float = compiler.platform.float_abi == FLOAT_ABI_SOFT;
	// Build target override.
	if (target->feature.soft_float != SOFT_FLOAT_DEFAULT)
	{
		compiler.platform.x86.use_soft_float = target->feature.soft_float == SOFT_FLOAT_YES;
	}

	compiler.platform.x86.is_mcu_api = compiler.platform.os == OS_TYPE_ELFIAMCU;
	switch (compiler.platform.os)
	{
		case OS_DARWIN_TYPES:
		case OS_TYPE_DRAGON_FLY:
		case OS_TYPE_FREEBSD:
		case OS_TYPE_ELFIAMCU:
		case OS_TYPE_OPENBSD:
		case OS_TYPE_WIN32:
			compiler.platform.x86.return_small_struct_in_reg_abi = true;
			break;
		default:
			break;
	}
	if (target->feature.x86_struct_return != STRUCT_RETURN_DEFAULT)
	{
		compiler.platform.x86.return_small_struct_in_reg_abi = target->feature.x86_struct_return == STRUCT_RETURN_REG;
	}
}


static void cpu_features_add_feature_single(CpuFeatures *cpu_features, int feature)
{
	if (feature < 64)
	{
		cpu_features->bits[0] |= 1ULL << feature;
	}
	else
	{
		cpu_features->bits[1] |= 1ULL << (feature - 64);
	}
}


static X86Feature x86feature_from_string(const char *str)
{
	for (int i = 0; i <= X86_FEATURE_LAST; i++)
	{
		const char *feature = x86_feature_name[i];
		for (int j = 0;; j++)
		{
			char c = str[j];
			if (feature[j] != c) goto NEXT;
			if (c == 0) return i;
		}
		NEXT:;
	}
	return -1;
}

static bool cpu_features_contains(CpuFeatures *cpu_features, int feature)
{
	if (feature < 64)
	{
		return !!((cpu_features->bits[0]) & (1ULL << feature));
	}
	return !!((cpu_features->bits[1]) & (1ULL << (feature - 64)));
}

static CpuFeatures x86_features_from_cpu(X86CpuSet set)
{
	CpuFeatures features = cpu_feature_zero;
	switch (set)
	{
		case X86CPU_NATIVE:
			x86_features_from_host(&features);
			break;
		case X86CPU_AVX512:
			x86_features_add_feature(&features, X86_FEAT_AVX512BW);
			x86_features_add_feature(&features, X86_FEAT_AVX512CD);
			x86_features_add_feature(&features, X86_FEAT_AVX512DQ);
			x86_features_add_feature(&features, X86_FEAT_AVX512VL);
			FALLTHROUGH;
		case X86CPU_AVX2_V1:
		case X86CPU_AVX2_V2:
			x86_features_add_feature(&features, X86_FEAT_AVX2);
			x86_features_add_feature(&features, X86_FEAT_BMI2);
			x86_features_add_feature(&features, X86_FEAT_BMI);
			x86_features_add_feature(&features, X86_FEAT_F16C);
			x86_features_add_feature(&features, X86_FEAT_FMA);
			x86_features_add_feature(&features, X86_FEAT_LZCNT);
			x86_features_add_feature(&features, X86_FEAT_MOVBE);
			x86_features_add_feature(&features, X86_FEAT_XSAVE);
			FALLTHROUGH;
		case X86CPU_SSE4:
		case X86CPU_AVX1:
			x86_features_add_feature(&features, X86_FEAT_SAHF);
			x86_features_add_feature(&features, X86_FEAT_POPCNT);
			x86_features_add_feature(&features, X86_FEAT_CRC32);
			x86_features_add_feature(&features, X86_FEAT_SSE4_2);
			x86_features_add_feature(&features, X86_FEAT_CMPXCHG16B);
			FALLTHROUGH;
		case X86CPU_SSSE3:
		case X86CPU_DEFAULT:
		case X86CPU_BASELINE:
			x86_features_add_feature(&features, X86_FEAT_MMX);
			x86_features_add_feature(&features, X86_FEAT_SSE3);
			x86_features_add_feature(&features, X86_FEAT_SSE2);
			x86_features_add_feature(&features, X86_FEAT_SSE);
			x86_features_add_feature(&features, X86_FEAT_CMOV);
			x86_features_add_feature(&features, X86_FEAT_FXSR);
			x86_features_add_feature(&features, X86_FEAT_CMPXCHG8B);
			break;
	}
	return features;
}

static void cpu_features_set_to_features(CpuFeatures enabled_features, CpuFeatures cpu_defaults, const bool *skip, const char *list[], int last)
{
	scratch_buffer_clear();
	for (int i = 0; i <= last; i++)
	{
		if (skip && skip[i]) continue;
		bool diff_has = cpu_features_contains(&cpu_defaults, i);
		if (cpu_features_contains(&enabled_features, i))
		{
			if (diff_has) continue;
			scratch_buffer_append_char('+');
		}
		else
		{
			if (!diff_has) continue;
			scratch_buffer_append_char('-');
		}
		scratch_buffer_append(list[i]);
		scratch_buffer_append_char(',');
	}
	if (scratch_buffer.len > 0) scratch_buffer.len--;
	compiler.platform.features = scratch_buffer_copy();
}

static void x86features_as_diff_to_scratch(CpuFeatures enabled_features, X86CpuSet set)
{
	static bool x64features_skip[X86_FEATURE_LAST + 1] = { [X86_FEAT_AVX5124FMAPS] = true, [X86_FEAT_AVX5124VNNIW] = true};
	cpu_features_set_to_features(enabled_features, x86_features_from_cpu(set), x64features_skip, x86_feature_name, X86_FEATURE_LAST);
}

static void x86_features_add_feature(CpuFeatures *cpu_features, X86Feature feature)
{
	cpu_features_add_feature_single(cpu_features, feature);
	switch (feature)
	{
		case X86_FEAT_SSE2:
		case X86_FEAT_GFNI:
		case X86_FEAT_PCLMUL:
			x86_features_add_feature(cpu_features, X86_FEAT_SSE);
			return;
		case X86_FEAT_AES:
		case X86_FEAT_SSE3:
		case X86_FEAT_SHA:
		case X86_FEAT_KL:
			x86_features_add_feature(cpu_features, X86_FEAT_SSE2);
			return;
		case X86_FEAT_SSE4_1:
			x86_features_add_feature(cpu_features, X86_FEAT_SSSE3);
			return;
		case X86_FEAT_SSE4_A:
		case X86_FEAT_SSSE3:
			x86_features_add_feature(cpu_features, X86_FEAT_SSE3);
			return;
		case X86_FEAT_SSE4_2:
			x86_features_add_feature(cpu_features, X86_FEAT_SSE4_1);
			return;
		case X86_FEAT_AVX:
			x86_features_add_feature(cpu_features, X86_FEAT_SSE4_2);
			return;
		case X86_FEAT_AVX512F:
			x86_features_add_feature(cpu_features, X86_FEAT_AVX2);
			x86_features_add_feature(cpu_features, X86_FEAT_F16C);
			x86_features_add_feature(cpu_features, X86_FEAT_FMA);
			return;
		case X86_FEAT_AVX2:
		case X86_FEAT_F16C:
		case X86_FEAT_FMA:
		case X86_FEAT_SM3:
			x86_features_add_feature(cpu_features, X86_FEAT_AVX);
			return;
		case X86_FEAT_VAES:
			x86_features_add_feature(cpu_features, X86_FEAT_AES);
			x86_features_add_feature(cpu_features, X86_FEAT_AVX);
			return;
		case X86_FEAT_VPCLMULQDQ:
			x86_features_add_feature(cpu_features, X86_FEAT_AVX);
			x86_features_add_feature(cpu_features, X86_FEAT_PCLMUL);
			return;
		case X86_FEAT_XSAVEC:
		case X86_FEAT_XSAVEOPT:
		case X86_FEAT_XSAVES:
			x86_features_add_feature(cpu_features, X86_FEAT_XSAVE);
			return;
		case X86_FEAT_AVX512CD:
		case X86_FEAT_AVX512BW:
		case X86_FEAT_AVX512DQ:
		case X86_FEAT_AVX512ER:
		case X86_FEAT_AVX512PF:
		case X86_FEAT_AVX512VL:
		case X86_FEAT_AVX512IFMA:
		case X86_FEAT_AVX512VNNI:
		case X86_FEAT_AVX512VPOPCNTDQ:
		case X86_FEAT_AVX512VP2INTERSECT:
			x86_features_add_feature(cpu_features, X86_FEAT_AVX512F);
			return;
		case X86_FEAT_FMA4:
			x86_features_add_feature(cpu_features, X86_FEAT_SSE4_A);
			x86_features_add_feature(cpu_features, X86_FEAT_AVX);
			return;
		case X86_FEAT_XOP:
			x86_features_add_feature(cpu_features, X86_FEAT_FMA4);
			return;
		case X86_FEAT_AMX_BF16:
		case X86_FEAT_AMX_FP16:
		case X86_FEAT_AMX_FP8:
		case X86_FEAT_AMX_INT8:
		case X86_FEAT_AMX_COMPLEX:
		case X86_FEAT_AMX_TRANSPOSE:
		case X86_FEAT_AMX_TF32:
		case X86_FEAT_AMX_MOVRS:
		case X86_FEAT_AMX_AVX512:
			x86_features_add_feature(cpu_features, X86_FEAT_AMX_TILE);
			return;
		case X86_FEAT_AVXVNNIINT8:
		case X86_FEAT_AVXVNNIINT16:
		case X86_FEAT_AVXIFMA:
		case X86_FEAT_AVXNECONVERT:
		case X86_FEAT_AVXVNNI:
		case X86_FEAT_SHA512:
		case X86_FEAT_SM4:
			x86_features_add_feature(cpu_features, X86_FEAT_AVX2);
			return;
		case X86_FEAT_AVX512FP16:
			x86_features_add_feature(cpu_features, X86_FEAT_AVX512BW);
			x86_features_add_feature(cpu_features, X86_FEAT_AVX512DQ);
			x86_features_add_feature(cpu_features, X86_FEAT_AVX512VL);
			return;
		case X86_FEAT_WIDEKL:
			x86_features_add_feature(cpu_features, X86_FEAT_KL);
			return;
		case X86_FEAT_AVX512VBMI:
		case X86_FEAT_AVX512VBMI2:
		case X86_FEAT_AVX512BITALG:
		case X86_FEAT_AVX512BF16:
			x86_features_add_feature(cpu_features, X86_FEAT_AVX512BW);
			return;
		case X86_FEAT_AVX5124VNNIW:
		case X86_FEAT_AVX5124FMAPS:
			// Not implemented in LLVM
			return;
		case X86_FEAT_ADX:
		case X86_FEAT_AMX_TILE:
		case X86_FEAT_BMI:
		case X86_FEAT_BMI2:
		case X86_FEAT_CLDEMOTE:
		case X86_FEAT_CLFLUSHOPT:
		case X86_FEAT_CLWB:
		case X86_FEAT_CLZERO:
		case X86_FEAT_CMOV:
		case X86_FEAT_CMPXCHG16B:
		case X86_FEAT_CMPXCHG8B:
		case X86_FEAT_CMPCCXADD:
		case X86_FEAT_CRC32:
		case X86_FEAT_ENQCMD:
		case X86_FEAT_FSGSBASE:
		case X86_FEAT_FXSR:
		case X86_FEAT_HRESET:
		case X86_FEAT_INVPCID:
		case X86_FEAT_LWP:
		case X86_FEAT_LZCNT:
		case X86_FEAT_MWAITX:
		case X86_FEAT_MMX:
		case X86_FEAT_MOVBE:
		case X86_FEAT_MOVDIR64B:
		case X86_FEAT_MOVDIRI:
		case X86_FEAT_MOVRS:
		case X86_FEAT_PCONFIG:
		case X86_FEAT_POPCNT:
		case X86_FEAT_PKU:
		case X86_FEAT_PREFETCHI:
		case X86_FEAT_PREFETCHWT1:
		case X86_FEAT_PRFCHW:
		case X86_FEAT_PTWRITE:
		case X86_FEAT_RAOINT:
		case X86_FEAT_RDPID:
		case X86_FEAT_RDPRU:
		case X86_FEAT_RDRND:
		case X86_FEAT_RDSEED:
		case X86_FEAT_RTM:
		case X86_FEAT_SSE:
		case X86_FEAT_SAHF:
		case X86_FEAT_SERIALIZE:
		case X86_FEAT_SGX:
		case X86_FEAT_SHSTK:
		case X86_FEAT_TBM:
		case X86_FEAT_TSXLDTRK:
		case X86_FEAT_UINTR:
		case X86_FEAT_VZEROUPPER:
		case X86_FEAT_WAITPKG:
		case X86_FEAT_WBNOINVD:
		case X86_FEAT_X87:
		case X86_FEAT_XSAVE:
		case X86_FEAT_EVEX512:
		case X86_FEAT_USERMSR:
		case X86_FEAT_APX_EGPR:
		case X86_FEAT_APX_PUSH2POP2:
		case X86_FEAT_APX_PPX:
		case X86_FEAT_APX_NDD:
		case X86_FEAT_APX_CCMP:
		case X86_FEAT_APX_NF:
		case X86_FEAT_APX_CF:
		case X86_FEAT_APX_ZU:
			return;
		case X86_FEAT_AVX10_1_512:
			x86_features_add_feature(cpu_features, X86_FEAT_AVX10_1_256);
			x86_features_add_feature(cpu_features, X86_FEAT_EVEX512);
		case X86_FEAT_AVX10_2_512:
			x86_features_add_feature(cpu_features, X86_FEAT_AVX10_1_256);
			x86_features_add_feature(cpu_features, X86_FEAT_EVEX512);
			return;
		case X86_FEAT_AVX10_1_256:
		case X86_FEAT_AVX10_2_256:
			x86_features_add_feature(cpu_features, X86_FEAT_AVX512CD);
			x86_features_add_feature(cpu_features, X86_FEAT_AVX512VBMI);
			x86_features_add_feature(cpu_features, X86_FEAT_AVX512VBMI2);
			x86_features_add_feature(cpu_features, X86_FEAT_AVX512IFMA);
			x86_features_add_feature(cpu_features, X86_FEAT_AVX512VNNI);
			x86_features_add_feature(cpu_features, X86_FEAT_AVX512BF16);
			x86_features_add_feature(cpu_features, X86_FEAT_AVX512VPOPCNTDQ);
			x86_features_add_feature(cpu_features, X86_FEAT_AVX512BITALG);
			x86_features_add_feature(cpu_features, X86_FEAT_VAES);
			x86_features_add_feature(cpu_features, X86_FEAT_VPCLMULQDQ);
			x86_features_add_feature(cpu_features, X86_FEAT_AVX512FP16);
			return;
		case X86_FEAT_SOFT_FLOAT:
			return;
	}
	UNREACHABLE_VOID
}


static void cpu_features_remove_feature(CpuFeatures *cpu_features, int feature)
{
	if (feature < 64)
	{
		cpu_features->bits[0] &= ~(1ULL << feature);
	}
	else
	{
		cpu_features->bits[1] &= ~(1ULL << (feature - 64));
	}
}

static void x64features_limit_from_capability(CpuFeatures *cpu_features, X86VectorCapability capability)
{
	switch (capability)
	{
		case X86VECTOR_NONE:
			cpu_features_remove_feature(cpu_features, X86_FEAT_MMX);
			FALLTHROUGH;
		case X86VECTOR_MMX:
			cpu_features_remove_feature(cpu_features, X86_FEAT_SSE);
			cpu_features_remove_feature(cpu_features, X86_FEAT_SSE2);
			cpu_features_remove_feature(cpu_features, X86_FEAT_SSE3);
			cpu_features_remove_feature(cpu_features, X86_FEAT_SSSE3);
			cpu_features_remove_feature(cpu_features, X86_FEAT_SSE4_1);
			cpu_features_remove_feature(cpu_features, X86_FEAT_SSE4_2);
			cpu_features_remove_feature(cpu_features, X86_FEAT_SSE4_A);
			cpu_features_remove_feature(cpu_features, X86_FEAT_AES);
			cpu_features_remove_feature(cpu_features, X86_FEAT_GFNI);
			cpu_features_remove_feature(cpu_features, X86_FEAT_KL);
			cpu_features_remove_feature(cpu_features, X86_FEAT_WIDEKL);
			cpu_features_remove_feature(cpu_features, X86_FEAT_PCLMUL);
			cpu_features_remove_feature(cpu_features, X86_FEAT_SHA);
			FALLTHROUGH;
		case X86VECTOR_SSE:
			cpu_features_remove_feature(cpu_features, X86_FEAT_AVX);
			cpu_features_remove_feature(cpu_features, X86_FEAT_AVX2);
			cpu_features_remove_feature(cpu_features, X86_FEAT_AVXNECONVERT);
			cpu_features_remove_feature(cpu_features, X86_FEAT_AVXVNNI);
			cpu_features_remove_feature(cpu_features, X86_FEAT_AVXIFMA);
			cpu_features_remove_feature(cpu_features, X86_FEAT_AVXVNNIINT8);
			cpu_features_remove_feature(cpu_features, X86_FEAT_F16C);
			cpu_features_remove_feature(cpu_features, X86_FEAT_FMA);
			cpu_features_remove_feature(cpu_features, X86_FEAT_FMA4);
			cpu_features_remove_feature(cpu_features, X86_FEAT_VAES);
			cpu_features_remove_feature(cpu_features, X86_FEAT_VPCLMULQDQ);
			cpu_features_remove_feature(cpu_features, X86_FEAT_XOP);
			FALLTHROUGH;
		case X86VECTOR_AVX:
			cpu_features_remove_feature(cpu_features, X86_FEAT_AVX512DQ);
			cpu_features_remove_feature(cpu_features, X86_FEAT_AVX512BW);
			cpu_features_remove_feature(cpu_features, X86_FEAT_AVX512CD);
			cpu_features_remove_feature(cpu_features, X86_FEAT_AVX512VL);
			cpu_features_remove_feature(cpu_features, X86_FEAT_AVX512ER);
			cpu_features_remove_feature(cpu_features, X86_FEAT_AVX512F);
			cpu_features_remove_feature(cpu_features, X86_FEAT_AVX512FP16);
			cpu_features_remove_feature(cpu_features, X86_FEAT_AVX512BF16);
			cpu_features_remove_feature(cpu_features, X86_FEAT_AVX5124FMAPS);
			cpu_features_remove_feature(cpu_features, X86_FEAT_AVX512BITALG);
			cpu_features_remove_feature(cpu_features, X86_FEAT_AVX5124VNNIW);
			cpu_features_remove_feature(cpu_features, X86_FEAT_AVX512VNNI);
			cpu_features_remove_feature(cpu_features, X86_FEAT_AVX512IFMA);
			cpu_features_remove_feature(cpu_features, X86_FEAT_AVX512VBMI);
			cpu_features_remove_feature(cpu_features, X86_FEAT_AVX512VBMI2);
			cpu_features_remove_feature(cpu_features, X86_FEAT_AVX512PF);
			cpu_features_remove_feature(cpu_features, X86_FEAT_AVX512VP2INTERSECT);
			cpu_features_remove_feature(cpu_features, X86_FEAT_AVX512VPOPCNTDQ);
			FALLTHROUGH;
		case X86VECTOR_AVX512:
			cpu_features_remove_feature(cpu_features, X86_FEAT_EVEX512);
			cpu_features_remove_feature(cpu_features, X86_FEAT_AVX10_1_512);
			cpu_features_remove_feature(cpu_features, X86_FEAT_AVX10_1_256);
			cpu_features_remove_feature(cpu_features, X86_FEAT_USERMSR);
			break;
		case X86VECTOR_CPU:
		case X86VECTOR_DEFAULT:
			break;
	}
}

static const char *x86_cpu_from_set(X86CpuSet set)
{
	switch (set)
	{
		case X86CPU_DEFAULT:
		case X86CPU_BASELINE:
		case X86CPU_SSSE3:
			return "x86-64";
		case X86CPU_SSE4:
		case X86CPU_AVX1:
			return "x86-64-v2";
		case X86CPU_AVX2_V1:
		case X86CPU_AVX2_V2:
			return "x86-64-v3";
		case X86CPU_AVX512:
			return "x86-64-v4";
		case X86CPU_NATIVE:
#if LLVM_AVAILABLE
			return LLVMGetHostCPUName();
#else
			return hostinfo_x86_cpu_name();
#endif
	}
	UNREACHABLE
}

static void x86_features_from_host(CpuFeatures *cpu_features)
{
#if LLVM_AVAILABLE
	char *features = LLVMGetHostCPUFeatures();
	INFO_LOG("Detected the following host features: %s", features);
	INFO_LOG("For %s", LLVMGetHostCPUName());

	char *tok = strtok(features, ",");
	*cpu_features = cpu_feature_zero;
	while (tok != NULL)
	{
		if (tok[0] == '-')
		{
			int i = x86feature_from_string(&tok[1]);
			if (i < 0)
			{
				if (debug_log || PRERELEASE) printf("WARNING, unknown feature %s - skipping\n", &tok[1]);
				goto NEXT;
			}
			cpu_features_remove_feature(cpu_features, (X86Feature)i);
		}
		else if (tok[0] == '+')
		{
			int i = x86feature_from_string(&tok[1]);
			if (i < 0)
			{
				// Ignore "64bit"
				if (strlen(&tok[1]) == 5 && memcmp(&tok[1], "64bit", 5) == 0) goto NEXT;
				if (debug_log || PRERELEASE) printf("WARNING, unknown feature %s - skipping\n", &tok[1]);
				goto NEXT;
			}
			x86_features_add_feature(cpu_features, (X86Feature)i);
		}
		NEXT:
		tok = strtok(NULL, ",");
	}
	LLVMDisposeMessage(features);
#else
	hostinfo_x86_features(cpu_features);
#endif
}

static void x86features_from_cpu(CpuFeatures *cpu_features, X86CpuSet cpu_set)
{
	*cpu_features = cpu_feature_zero;
	switch (cpu_set)
	{
		case X86CPU_AVX512: // Knl, Skylake, Cascade and up, Amd Zen 4+
			x86_features_add_feature(cpu_features, X86_FEAT_AVX512F);
			x86_features_add_feature(cpu_features, X86_FEAT_AVX512CD);
			x86_features_add_feature(cpu_features, X86_FEAT_AVX512VPOPCNTDQ);
			x86_features_add_feature(cpu_features, X86_FEAT_AVX512BW);
			x86_features_add_feature(cpu_features, X86_FEAT_AVX512DQ);
			x86_features_add_feature(cpu_features, X86_FEAT_AVX512BF16);
			x86_features_add_feature(cpu_features, X86_FEAT_AVX512VL);
			x86_features_add_feature(cpu_features, X86_FEAT_AVX512IFMA);
			x86_features_add_feature(cpu_features, X86_FEAT_SHA);
			x86_features_add_feature(cpu_features, X86_FEAT_AVX512BITALG);
			x86_features_add_feature(cpu_features, X86_FEAT_AVX512VBMI);
			x86_features_add_feature(cpu_features, X86_FEAT_AVX512VBMI2);
			x86_features_add_feature(cpu_features, X86_FEAT_AVXVNNI);
			x86_features_add_feature(cpu_features, X86_FEAT_GFNI);
			x86_features_add_feature(cpu_features, X86_FEAT_RDPID);
			x86_features_add_feature(cpu_features, X86_FEAT_VAES);
			x86_features_add_feature(cpu_features, X86_FEAT_VPCLMULQDQ);
			x86_features_add_feature(cpu_features, X86_FEAT_PKU);
			FALLTHROUGH;
		case X86CPU_AVX2_V2: // Zen1+ Alderlake+, Skylake+
			x86_features_add_feature(cpu_features, X86_FEAT_AES);
			x86_features_add_feature(cpu_features, X86_FEAT_ADX);
			x86_features_add_feature(cpu_features, X86_FEAT_RDSEED);
			x86_features_add_feature(cpu_features, X86_FEAT_PRFCHW);
			x86_features_add_feature(cpu_features, X86_FEAT_XSAVES);
			x86_features_add_feature(cpu_features, X86_FEAT_XSAVEC);
			x86_features_add_feature(cpu_features, X86_FEAT_CLFLUSHOPT);
			FALLTHROUGH;
		case X86CPU_AVX2_V1: // Haswell+, Bdver4+
			x86_features_add_feature(cpu_features, X86_FEAT_AVX2);
			x86_features_add_feature(cpu_features, X86_FEAT_BMI);
			x86_features_add_feature(cpu_features, X86_FEAT_BMI2);
			x86_features_add_feature(cpu_features, X86_FEAT_F16C);
			x86_features_add_feature(cpu_features, X86_FEAT_FMA);
			x86_features_add_feature(cpu_features, X86_FEAT_LZCNT);
			x86_features_add_feature(cpu_features, X86_FEAT_MOVBE);
			x86_features_add_feature(cpu_features, X86_FEAT_XSAVEOPT);
			x86_features_add_feature(cpu_features, X86_FEAT_PCLMUL);
			x86_features_add_feature(cpu_features, X86_FEAT_FSGSBASE);
			x86_features_add_feature(cpu_features, X86_FEAT_RDRND);
			x86_features_add_feature(cpu_features, X86_FEAT_LZCNT);
			FALLTHROUGH;
		case X86CPU_AVX1:
			x86_features_add_feature(cpu_features, X86_FEAT_AVX);
			x86_features_add_feature(cpu_features, X86_FEAT_PCLMUL);
			x86_features_add_feature(cpu_features, X86_FEAT_XSAVE);
			FALLTHROUGH;
		case X86CPU_SSE4:
			x86_features_add_feature(cpu_features, X86_FEAT_SSE4_1);
			x86_features_add_feature(cpu_features, X86_FEAT_SSE4_2);
			x86_features_add_feature(cpu_features, X86_FEAT_POPCNT);
			x86_features_add_feature(cpu_features, X86_FEAT_CRC32);
			FALLTHROUGH;
		case X86CPU_SSSE3: // Basically Penryn+, Amdfam10
			x86_features_add_feature(cpu_features, X86_FEAT_SAHF);
			x86_features_add_feature(cpu_features, X86_FEAT_SSSE3);
			x86_features_add_feature(cpu_features, X86_FEAT_CMPXCHG16B);
			FALLTHROUGH;
		case X86CPU_DEFAULT:
		case X86CPU_BASELINE: // K8+
			x86_features_add_feature(cpu_features, X86_FEAT_CMPXCHG8B);
			x86_features_add_feature(cpu_features, X86_FEAT_MMX);
			x86_features_add_feature(cpu_features, X86_FEAT_FXSR);
			x86_features_add_feature(cpu_features, X86_FEAT_SSE);
			x86_features_add_feature(cpu_features, X86_FEAT_SSE2);
			x86_features_add_feature(cpu_features, X86_FEAT_CMOV);
			return;
		case X86CPU_NATIVE:
			x86_features_from_host(cpu_features);
			return;
	}
	UNREACHABLE_VOID
}

static inline bool cpu_has_all_features(CpuFeatures *feature_to_test, CpuFeatures *features_to_match)
{
	return (feature_to_test->bits[0] & features_to_match->bits[0]) == features_to_match->bits[0]
		&& (feature_to_test->bits[1] & features_to_match->bits[1]) == features_to_match->bits[1];
}

static X86CpuSet x64_cpu_default(void)
{
	CpuFeatures features;
	x86_features_from_host(&features);
	CpuFeatures other_features;
	x86features_from_cpu(&other_features, X86CPU_AVX2_V1);
	if (cpu_has_all_features(&features, &other_features)) return X86CPU_AVX2_V1;
	x86features_from_cpu(&other_features, X86CPU_AVX1);
	if (cpu_has_all_features(&features, &other_features)) return X86CPU_AVX1;
	x86features_from_cpu(&other_features, X86CPU_SSE4);
	if (cpu_has_all_features(&features, &other_features)) return X86CPU_SSE4;
	x86features_from_cpu(&other_features, X86CPU_SSSE3);
	if (cpu_has_all_features(&features, &other_features)) return X86CPU_SSSE3;
	return X86CPU_BASELINE;
}

static inline void target_setup_x64_abi(BuildTarget *target)
{
	compiler.platform.abi = ABI_X64;
	X86CpuSet cpu_set;
	compiler.platform.x64.is_win64 = compiler.platform.os == OS_TYPE_WIN32;
	bool is_native = target->arch_os_target == default_target;
	if (!is_native && target->feature.x86_cpu_set == X86CPU_NATIVE) target->feature.x86_cpu_set = X86CPU_DEFAULT;
	if (target->feature.x86_cpu_set != X86CPU_DEFAULT)
	{
		cpu_set = target->feature.x86_cpu_set;
	}
	else
	{
		cpu_set = is_native ? x64_cpu_default() : X86CPU_AVX1;
		if (cpu_set > X86CPU_AVX1) cpu_set = X86CPU_AVX1;
		INFO_LOG("Set default CPU as %s\n", x86_cpu_set[cpu_set]);
	}

	compiler.platform.cpu = x86_cpu_from_set(cpu_set);
	CpuFeatures cpu_features;
	x86features_from_cpu(&cpu_features, cpu_set);
	x64features_limit_from_capability(&cpu_features, target->feature.x86_vector_capability);
	if (target->feature.soft_float == SOFT_FLOAT_YES) compiler.platform.x64.soft_float = true;
	if (target->feature.pass_win64_simd_as_arrays == WIN64_SIMD_ARRAY) compiler.platform.x64.win64_simd_as_array = true;
	if (compiler.platform.x64.soft_float) cpu_features_add_feature_single(&cpu_features, X86_FEAT_SOFT_FLOAT);

	update_cpu_features(compiler.build.cpu_flags, &cpu_features, x86_feature_name, X86_FEATURE_LAST);

	x86features_as_diff_to_scratch(cpu_features, cpu_set);

	if (compiler.platform.environment_type == ENV_TYPE_GNU)
	{
		//platform_target.x64.is_mingw64 = platform_target.x64.is_win64;
		if (compiler.platform.x64.is_win64) DEBUG_LOG("Mingw");
	}
	if (compiler.platform.os == OS_TYPE_LINUX || compiler.platform.os == OS_TYPE_NETBSD)
	{
		compiler.platform.x64.pass_int128_vector_in_mem = true;
	}
	compiler.platform.x64.features = cpu_features;
	if (cpu_features_contains(&cpu_features, X86_FEAT_AVX512F))
	{
		compiler.platform.x64.native_vector_size_avx = 64;
		compiler.platform.x64.align_simd_default = 512;
	}
	else if (cpu_features_contains(&cpu_features, X86_FEAT_AVX))
	{
		compiler.platform.x64.native_vector_size_avx = 32;
		compiler.platform.x64.align_simd_default = 256;
	}
	else if (cpu_features_contains(&cpu_features, X86_FEAT_SSE))
	{
		compiler.platform.x64.native_vector_size_avx = 16;
		compiler.platform.x64.align_simd_default = 128;
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
			return "arm64";
		case ARCH_TYPE_AARCH64_BE:
			return "arm64e";
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
		case ARCH_TYPE_XTENSA:
			return "xtensa";
	}
	UNREACHABLE;
}

static char *arch_to_target_triple(ArchOsTarget target, LinuxLibc linux_libc)
{
	switch (target)
	{
		case FREEBSD_X86: return "i386-unknown-freebsd";
		case OPENBSD_X86: return "i386-unknown-openbsd";
		case NETBSD_X86: return "i386-unknown-netbsd";
		case MCU_X86: return "i386-pc-elfiamcu";
		case LINUX_X86: return linux_libc == LINUX_LIBC_MUSL ? "i386-unknown-linux-musl" : "i386-unknown-linux";
		case ELF_X86: return "i386-unknown-elf";
		case MACOS_X64: return "x86_64-apple-macosx";
		case LINUX_X64: return linux_libc == LINUX_LIBC_MUSL ? "x86_64-pc-linux-musl" : "x86_64-pc-linux-gnu";
		case WINDOWS_X64: return "x86_64-pc-windows-msvc";
		case MINGW_X64: return "x86_64-w64-windows-gnu";
		case NETBSD_X64: return "x86_64-pc-netbsd";
		case FREEBSD_X64: return "x86_64-pc-freebsd";
		case OPENBSD_X64: return "x86_64-pc-openbsd";
		case ELF_X64: return "x86_64-unknown-elf";
		case ANDROID_AARCH64: return "aarch64-linux-android";
		case ANDROID_X86_64: return "x86_64-linux-android";
		case LINUX_AARCH64: return linux_libc == LINUX_LIBC_MUSL ? "aarch64-unknown-linux-musl" : "aarch64-unknown-linux-gnu";
		case IOS_AARCH64: return "aarch64-apple-ios";
		case MACOS_AARCH64: return "aarch64-apple-macosx";
		case ELF_AARCH64: return "aarch64-unknown-elf";
		case WINDOWS_AARCH64: return "aarch64-pc-windows-msvc";
		case LINUX_RISCV32: return linux_libc == LINUX_LIBC_MUSL ? "riscv32-unknown-linux-musl" : "riscv32-unknown-linux";
		case ELF_RISCV32: return "riscv32-unknown-elf";
		case LINUX_RISCV64: return linux_libc == LINUX_LIBC_MUSL ? "riscv64-unknown-linux-musl" : "riscv64-unknown-linux";
		case ELF_RISCV64: return "riscv64-unknown-elf";
		case ELF_XTENSA: return "xtensa-unknown-elf";
		case WASM32: return "wasm32-unknown-unknown";
		case WASM64: return "wasm64-unknown-unknown";
		case ARCH_OS_TARGET_DEFAULT: UNREACHABLE;
	}
	UNREACHABLE;
};

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
	STRCASE("xtensa", ARCH_TYPE_XTENSA)
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

	INFO_LOG("Platform Environment: %s", env.ptr);
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
	STRCASE("freebsd", OS_TYPE_FREEBSD)
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
	STRCASE("android", OS_TYPE_ANDROID)
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
		case ARCH_TYPE_XTENSA:
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
	}
	UNREACHABLE
}

static unsigned arch_int_register_bit_width(OsType os, ArchType arch)
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
		case ARCH_TYPE_XTENSA:
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
	}
	UNREACHABLE
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
		case ARCH_TYPE_RISCV64:
		case ARCH_TYPE_RISCV32:
			return true;
		case ARCH_TYPE_AARCH64:
			return false;
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

static ObjectFormatType object_format_from_os(OsType os, ArchType arch_type)
{
	switch (os)
	{
		case OS_UNSUPPORTED:
			return OBJ_FORMAT_UNSUPPORTED;
		case OS_TYPE_UNKNOWN:
		case OS_TYPE_NONE:
			if (arch_is_wasm(arch_type)) return OBJ_FORMAT_WASM;
			FALLTHROUGH;
		case OS_TYPE_LINUX:
		case OS_TYPE_ANDROID:
		case OS_TYPE_NETBSD:
		case OS_TYPE_OPENBSD:
		case OS_TYPE_FREEBSD:
			return OBJ_FORMAT_ELF;
		case OS_DARWIN_TYPES:
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
		case ARCH_TYPE_AARCH64:
		case ARCH_TYPE_WASM32:
		case ARCH_TYPE_WASM64:
			return true;
		case ARCH_TYPE_PPC:
		case ARCH_TYPE_XTENSA:
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
		case OS_DARWIN_TYPES:
		case OS_TYPE_LINUX:
		case OS_TYPE_ANDROID:
		case OS_TYPE_NONE:
		case OS_TYPE_FREEBSD:
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


static AlignData os_target_alignment_of_int(OsType os, ArchType arch, uint32_t bits)
{
	switch (arch)
	{
		case ARCH_TYPE_UNKNOWN:
		case ARCH_UNSUPPORTED:
			UNREACHABLE_VOID;
			return (AlignData) { 0, 0 };
		case ARCH_TYPE_ARM:
		case ARCH_TYPE_THUMB:
		case ARCH_TYPE_ARMB:
		case ARCH_TYPE_THUMBEB:
			if ((os_is_apple(os) || os == OS_TYPE_NETBSD) && bits > 32) return (AlignData) { 32, MIN(64u, bits) };
			return (AlignData) { MIN(64u, bits), MIN(64u, bits) };
		case ARCH_TYPE_PPC64:
		case ARCH_TYPE_PPC:
		case ARCH_TYPE_PPC64LE:
		case ARCH_TYPE_WASM64:
		case ARCH_TYPE_RISCV32:
		case ARCH_TYPE_WASM32:
		case ARCH_TYPE_XTENSA:
			return (AlignData) { MIN(64u, bits), MIN(64u, bits) };
		case ARCH_TYPE_X86_64:
#if LLVM_AVAILABLE && LLVM_VERSION_MAJOR < 18
			return (AlignData) { MIN(64u, bits), MIN(64u, bits) };
#else
			FALLTHROUGH;
#endif
		case ARCH_TYPE_RISCV64:
			return (AlignData) { bits, bits };
		case ARCH_TYPE_AARCH64:
		case ARCH_TYPE_AARCH64_BE:
			if (bits < 32) return (AlignData){ bits, 32 };
			return (AlignData) { bits, bits };
		case ARCH_TYPE_X86:
			if (bits <= 32) return (AlignData) { bits, bits };
#if !LLVM_AVAILABLE || LLVM_VERSION_MAJOR > 17
			if (bits == 128) return (AlignData) { 128, 128 };
#endif
			if (os == OS_TYPE_ELFIAMCU) return (AlignData) { 32, 32 };
			if (os == OS_TYPE_WIN32 || os == OS_TYPE_NACL) return (AlignData) { 64, 64 };
			return (AlignData) { 32, 64 };
	}
	UNREACHABLE_VOID;
	return (AlignData) { 0, 0 };
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
		case ARCH_TYPE_XTENSA:
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
			UNREACHABLE_VOID;
			break;
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
		case ARCH_TYPE_XTENSA:
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
	UNREACHABLE_VOID;
	return (AlignData) { 0, 0 };
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
				return RELOC_SMALL_PIC;
			case OS_DARWIN_TYPES:
				return RELOC_BIG_PIC;
			case OS_TYPE_WIN32:
				if (arch == ARCH_TYPE_X86) return RELOC_NONE;
				return RELOC_BIG_PIC;
			case OS_TYPE_LINUX:
			case OS_TYPE_ANDROID:
				return RELOC_BIG_PIC;
			case OS_TYPE_WASI:
				return RELOC_NONE;
			case OS_TYPE_FREEBSD:
			case OS_TYPE_NETBSD:
			case OS_TYPE_UNKNOWN:
			case OS_TYPE_NONE:
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
		case OS_TYPE_FREEBSD:
		case OS_TYPE_NETBSD:
			switch (arch)
			{
				case ARCH_TYPE_MIPS64:
				case ARCH_TYPE_MIPS64EL:
					return RELOC_SMALL_PIC;
				default:
					return RELOC_NONE;
			}
		case OS_DARWIN_TYPES:
			return RELOC_BIG_PIC;
		case OS_TYPE_WIN32:
			if (arch == ARCH_TYPE_X86) return RELOC_NONE;
			return RELOC_BIG_PIC;
		case OS_TYPE_WASI:
			return RELOC_NONE;
		case OS_TYPE_LINUX:
		case OS_TYPE_ANDROID:
			return RELOC_BIG_PIE;
		case OS_TYPE_OPENBSD:
			return RELOC_SMALL_PIE;
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
		case OS_TYPE_FREEBSD:
		case OS_TYPE_LINUX:
		case OS_TYPE_ANDROID:
		case OS_TYPE_NETBSD:
		case OS_TYPE_OPENBSD:
			return false;
		case OS_UNSUPPORTED:
			UNREACHABLE
	}
	UNREACHABLE
}

INLINE const char *llvm_macos_target_triple(const char *triple)
{
	if (compiler.build.macos.min_version)
	{
		scratch_buffer_clear();
		scratch_buffer_append(triple);
		scratch_buffer_append(compiler.build.macos.min_version);
		return scratch_buffer_to_string();
	}
	MacSDK *mac_sdk = compiler.build.macos.sdk;

	if (!mac_sdk)
	{
		scratch_buffer_clear();
		scratch_buffer_append(triple);
		scratch_buffer_append("10.15.0");
		return scratch_buffer_to_string();
	}
	scratch_buffer_clear();
	scratch_buffer_append(triple);
	scratch_buffer_printf("%d.%d.0", mac_sdk->macos_min_deploy_target.major, mac_sdk->macos_min_deploy_target.minor);
	return scratch_buffer_to_string();
}

#if LLVM_AVAILABLE
#define INITIALIZE_TARGET(X) do { \
  DEBUG_LOG("Initialize target: %s.", #X); \
  LLVMInitialize ## X ## AsmParser(); \
  LLVMInitialize ## X ## AsmPrinter(); \
  LLVMInitialize ## X ## TargetInfo(); \
  LLVMInitialize ## X ## Target(); \
  LLVMInitialize ## X ## Disassembler(); \
  LLVMInitialize ## X ## TargetMC(); \
 } while(0)

void *llvm_target_machine_create(void)
{
	static bool llvm_initialized = false;

	if (!llvm_initialized)
	{
		llvm_initialized = true;
#ifdef XTENSA_ENABLE
		INITIALIZE_TARGET(Xtensa);
#endif
#ifndef ARM_DISABLE
		INITIALIZE_TARGET(ARM);
#endif
#ifndef AARCH64_DISABLE
		INITIALIZE_TARGET(AArch64);
#endif
#ifndef RISCV_DISABLE
		INITIALIZE_TARGET(RISCV);
#endif
#ifndef WASM_DISABLE
		INITIALIZE_TARGET(WebAssembly);
#endif
#ifndef X86_DISABLE
		INITIALIZE_TARGET(X86);
#endif
		// To support more targets, add them above.
	}
	char *err = NULL;
	LLVMTargetRef target = NULL;
	if (LLVMGetTargetFromTriple(compiler.platform.target_triple, &target, &err) != 0)
	{
		error_exit("Could not create target: %s for triple '%s'", err, compiler.platform.target_triple);
		// Usually we would dispose of err, but no need to do it due to exit.
	}
	LLVMRelocMode reloc_mode = LLVMRelocDefault;

	switch (compiler.platform.reloc_model)
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
	INFO_LOG("CPU: %s", compiler.platform.cpu);
	INFO_LOG("Features: %s", compiler.platform.features);
	LLVMCodeModel model = compiler.build.kernel_build ? LLVMCodeModelKernel : LLVMCodeModelDefault;
	void *result = LLVMCreateTargetMachine(target, compiler.platform.target_triple,
										   compiler.platform.cpu ? compiler.platform.cpu : "", compiler.platform.features ? compiler.platform.features : "",
										   (LLVMCodeGenOptLevel)compiler.platform.llvm_opt_level,
										   reloc_mode, model);
	LLVMSetTargetMachineUseInitArray(result, true);
	if (!result) error_exit("Failed to create target machine.");
	LLVMSetTargetMachineAsmVerbosity(result, 1);
	return result;
}

#endif


static void target_setup_wasm_abi(BuildTarget *target)
{
	compiler.platform.abi = ABI_WASM;
	CpuFeatures features = cpu_feature_zero;
	cpu_features_add_feature_single(&features, WASM_FEAT_BULK_MEMORY);
#if LLVM_VERSION_MAJOR > 19
	cpu_features_add_feature_single(&features, WASM_FEAT_BULK_MEMORY_OPT);
	cpu_features_add_feature_single(&features, WASM_FEAT_CALL_INDIRECT_OVERLONG);
#endif
	cpu_features_add_feature_single(&features, WASM_FEAT_MULTIVALUE);
	cpu_features_add_feature_single(&features, WASM_FEAT_MUTABLE_GLOBALS);
	cpu_features_add_feature_single(&features, WASM_FEAT_NONTRAPPING_FPTORINT);
	cpu_features_add_feature_single(&features, WASM_FEAT_REFERENCE_TYPES);
	cpu_features_add_feature_single(&features, WASM_FEAT_SIGN_EXT);
	update_cpu_features(target->cpu_flags, &features, wasm_feature_name, WASM_FEATURE_LAST);
	cpu_features_set_to_features(features, cpu_feature_zero, NULL, wasm_feature_name, WASM_FEATURE_LAST);
}

static void target_setup_riscv_abi(BuildTarget *target)
{
	compiler.platform.riscv.xlen = arch_pointer_bit_width(compiler.platform.os, compiler.platform.arch) / 8; // pointer width
	RiscvCpuSet cpu = target->feature.riscv_cpu_set;
	compiler.platform.abi = ABI_RISCV;
	CpuFeatures features = cpu_feature_zero;
	if (compiler.platform.arch == ARCH_TYPE_RISCV64)
	{
		cpu_features_add_feature_single(&features, RISCV_FEAT_64BIT);
		if (cpu == RISCV_CPU_DEFAULT) cpu = RISCV_CPU_RVGC;
	}
	else
	{
		cpu_features_add_feature_single(&features, RISCV_FEAT_32BIT);
		if (cpu == RISCV_CPU_DEFAULT) cpu = RISCV_CPU_RVI;
	}
	if (target->feature.riscv_abi == RISCV_ABI_DEFAULT)
	{
		switch (cpu)
		{
			case RISCV_CPU_DEFAULT:
				UNREACHABLE_VOID
			case RISCV_CPU_RVI:
			case RISCV_CPU_RVIMAC:
				target->feature.riscv_abi = RISCV_ABI_INT_ONLY;
				break;
			case RISCV_CPU_RVIMAFC:
				target->feature.riscv_abi = RISCV_ABI_FLOAT;
				break;
			case RISCV_CPU_RVGC:
			case RISCV_CPU_RVGCV:
				target->feature.riscv_abi = RISCV_ABI_DOUBLE;
				break;
		}
	}
	switch (target->feature.riscv_abi)
	{
		case RISCV_ABI_DEFAULT:
			UNREACHABLE_VOID
		case RISCV_ABI_INT_ONLY:
		case RISCV_ABI_FLOAT:
		case RISCV_ABI_DOUBLE:
			compiler.platform.riscv.flen = 4 * target->feature.riscv_abi;
			break;
	}
	cpu_features_add_feature_single(&features, RISCV_FEAT_I);
	switch (cpu)
	{
		case RISCV_CPU_DEFAULT:
			UNREACHABLE_VOID
		case RISCV_CPU_RVI:
			break;
		case RISCV_CPU_RVGCV:
			cpu_features_add_feature_single(&features, RISCV_FEAT_V);
			cpu_features_add_feature_single(&features, RISCV_FEAT_ZFA);
			FALLTHROUGH;
		case RISCV_CPU_RVGC:
			cpu_features_add_feature_single(&features, RISCV_FEAT_D);
			cpu_features_add_feature_single(&features, RISCV_FEAT_ZICSR);
			cpu_features_add_feature_single(&features, RISCV_FEAT_ZIFENCEI);
			cpu_features_add_feature_single(&features, RISCV_FEAT_ZBA);
			cpu_features_add_feature_single(&features, RISCV_FEAT_ZBB);
			cpu_features_add_feature_single(&features, RISCV_FEAT_ZBS);
			FALLTHROUGH;
		case RISCV_CPU_RVIMAFC:
			cpu_features_add_feature_single(&features, RISCV_FEAT_F);
			FALLTHROUGH;
		case RISCV_CPU_RVIMAC:
			cpu_features_add_feature_single(&features, RISCV_FEAT_M);
			cpu_features_add_feature_single(&features, RISCV_FEAT_A);
			cpu_features_add_feature_single(&features, RISCV_FEAT_C);
			break;
	}
#if LLVM_VERSION_MAJOR < 18
	// Not supported prior to LLVM 18
	cpu_features_remove_feature(&features, RISCV_FEAT_I);
#endif
	update_cpu_features(target->cpu_flags, &features, riscv_feature_name, RISCV_FEATURE_LAST);
	cpu_features_set_to_features(features, cpu_feature_zero, NULL, riscv_feature_name, RISCV_FEATURE_LAST);
}

static void update_cpu_features(const char *features, CpuFeatures *cpu_features, const char **feature_names, int feature_count)
{
	if (!features) return;
	StringSlice slice = slice_from_string(features);
	if (!slice.len) return;
	while (slice.len)
	{
		StringSlice next = slice_next_token(&slice, ',');
		slice_trim(&next);
		if (!next.len) continue;
		bool add_feature = true;
		if (next.len < 2) goto ERR_MALFORMED;
		switch (*next.ptr)
		{
			case '+':
				break;
			case '-':
				add_feature = false;
				break;
			default:
				ERR_MALFORMED:
				error_exit("Invalid feature flag: %.*s in '%s'", (int)next.len, next.ptr, features);
		}
		next.ptr++;
		next.len--;
		for (int i = 0; i < feature_count; i++)
		{
			const char *feat = feature_names[i];
			size_t feat_len = strlen(feat);
			if (feat_len != next.len) continue;
			if (memcmp(next.ptr, feature_names[i], feat_len) == 0)
			{
				if (add_feature)
				{
					cpu_features_add_feature_single(cpu_features, i);
				}
				else
				{
					cpu_features_remove_feature(cpu_features, i);
				}
				goto FOUND;
			}
		}
		OUTF("Unsupported feature flag: '%.*s', will be ignored.", (int)next.len, next.ptr);
FOUND:;
	}
}


void target_setup(BuildTarget *target)
{
	if (target->win.def && !file_exists(target->win.def))
	{
		error_exit("Failed to find Windows def file: '%s' in path.", target->win.def);
	}

#ifndef XTENSA_ENABLE
	if (target->arch_os_target == ELF_XTENSA)
	{
		error_exit("Xtensa support is not available with this LLVM version.");
	}
#endif

	compiler.platform.target_triple = arch_to_target_triple(target->arch_os_target, target->linuxpaths.libc);
	ASSERT(compiler.platform.target_triple);

	compiler.platform.alloca_address_space = 0;

#if LLVM_AVAILABLE
	// Create a specific target machine
	LLVMCodeGenOptLevel level;

	switch (target->optlevel)
	{
		case OPTIMIZATION_NOT_SET:
			UNREACHABLE_VOID;
		case OPTIMIZATION_AGGRESSIVE:
			level = LLVMCodeGenLevelAggressive;
			break;
		case OPTIMIZATION_MORE:
			level = LLVMCodeGenLevelDefault;
			break;
		case OPTIMIZATION_LESS:
			level = LLVMCodeGenLevelLess;
			break;
		case OPTIMIZATION_NONE:
			level = LLVMCodeGenLevelNone;
			break;
		default:
			UNREACHABLE_VOID;
	}

	compiler.platform.llvm_opt_level = (int)level;
#endif

	INFO_LOG("Triple picked was %s.", compiler.platform.target_triple);

#if LLVM_AVAILABLE
	INFO_LOG("Default was %s.", LLVM_DEFAULT_TARGET_TRIPLE);
#else
	INFO_LOG("Default was %s.", hostinfo_default_triple());
#endif

	StringSlice target_triple_string = slice_from_string(compiler.platform.target_triple);
	compiler.platform.arch = arch_from_llvm_string(slice_next_token(&target_triple_string, '-'));
	if (!arch_is_supported(compiler.platform.arch))
	{
		OUTF("WARNING! This architecture is unsupported.\n");
	}
	compiler.platform.vendor = vendor_from_llvm_string(slice_next_token(&target_triple_string, '-'));
	compiler.platform.os = os_from_llvm_string(slice_next_token(&target_triple_string, '-'));
	compiler.platform.environment_type = environment_type_from_llvm_string(target_triple_string);
	if (compiler.platform.environment_type == ENV_TYPE_ANDROID) compiler.platform.os = OS_TYPE_ANDROID;

	if (target->debug_info == DEBUG_INFO_NOT_SET)
	{
		target->debug_info = DEBUG_INFO_FULL;
	}

	compiler.platform.float_abi = false;
	// TLS should not be supported for:
	// ARM Cygwin
	// NVPTX
	compiler.platform.tls_supported = os_target_use_thread_local(compiler.platform.os);
	compiler.platform.big_endian = arch_big_endian(compiler.platform.arch);
	compiler.platform.width_pointer = arch_pointer_bit_width(compiler.platform.os, compiler.platform.arch);
	compiler.platform.width_register = arch_int_register_bit_width(compiler.platform.os, compiler.platform.arch);
	compiler.platform.alloca_address_space = 0;
	compiler.platform.object_format = object_format_from_os(compiler.platform.os, compiler.platform.arch);
	switch (compiler.platform.object_format)
	{
		case OBJ_FORMAT_COFF:
		case OBJ_FORMAT_ELF:
		case OBJ_FORMAT_WASM:
			compiler.platform.use_comdat = true;
			break;
		default:
			break;
	}

	compiler.platform.int128 = os_target_supports_int128(compiler.platform.os, compiler.platform.arch);
	compiler.platform.vec128f = os_target_supports_vec(compiler.platform.os, compiler.platform.arch, 128, false);
	compiler.platform.vec128i = os_target_supports_vec(compiler.platform.os, compiler.platform.arch, 128, true);
	compiler.platform.vec64f = os_target_supports_vec(compiler.platform.os, compiler.platform.arch, 64, false);
	compiler.platform.vec64i = os_target_supports_vec(compiler.platform.os, compiler.platform.arch, 64, true);
	compiler.platform.float128 = os_target_supports_float128(compiler.platform.os, compiler.platform.arch);
	compiler.platform.float16 = os_target_supports_float16(compiler.platform.os, compiler.platform.arch);
	for (BitSizes i = BITS8; i < BITSIZES_LEN; i++)
	{
		unsigned bits = (unsigned) (8 << (i - 1));
		compiler.platform.integers[i] = os_target_alignment_of_int(compiler.platform.os, compiler.platform.arch, bits);
		compiler.platform.floats[i] = os_target_alignment_of_float(compiler.platform.os, compiler.platform.arch, bits);
	}
	compiler.platform.integers[BIT1] = compiler.platform.integers[BITS8];

	compiler.platform.align_pointer = (AlignData) { compiler.platform.width_pointer, compiler.platform.width_pointer };
	compiler.platform.width_c_short = os_target_c_type_bits(compiler.platform.os, compiler.platform.arch, CTYPE_SHORT);
	compiler.platform.width_c_int = os_target_c_type_bits(compiler.platform.os, compiler.platform.arch, CTYPE_INT);
	compiler.platform.width_c_long = os_target_c_type_bits(compiler.platform.os, compiler.platform.arch, CTYPE_LONG);
	compiler.platform.width_c_long_long = os_target_c_type_bits(compiler.platform.os, compiler.platform.arch, CTYPE_LONG_LONG);
	compiler.platform.signed_c_char = os_target_signed_c_char_type(compiler.platform.os, compiler.platform.arch);
	switch (compiler.platform.arch)
	{
		case ARCH_UNSUPPORTED:
			UNREACHABLE_VOID
			break;
		case ARCH_TYPE_AARCH64:
		case ARCH_TYPE_AARCH64_BE:
			target_setup_aarch64_abi();
			break;
		case ARCH_TYPE_XTENSA:
			compiler.platform.abi = ABI_XTENSA;
			break;
		case ARCH_TYPE_WASM32:
		case ARCH_TYPE_WASM64:
			target_setup_wasm_abi(target);
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
			if (compiler.platform.object_format != OBJ_FORMAT_ELF)
			{
				if (compiler.platform.arch == ARCH_TYPE_PPC64LE)
				{
					FATAL_ERROR("PPC64 LE non-ELF not supported.");
				}
				FATAL_ERROR("PPC64 not supported");
			}
			/** Here we need to have different codegen depending on elf version :( */
			compiler.platform.abi = ABI_PPC64_SVR4;
			compiler.platform.ppc64.is_softfp = compiler.platform.float_abi == FLOAT_ABI_SOFT;
			/* todo enable if elfv2 */
			compiler.platform.ppc64.is_elfv2 = compiler.platform.arch == ARCH_TYPE_PPC64LE;
			/* todo enable if elfv1-qpx */
			compiler.platform.ppc64.has_qpx = false;
			break;
		case ARCH_TYPE_RISCV64:
		case ARCH_TYPE_RISCV32:
			target_setup_riscv_abi(target);
			break;
		case ARCH_TYPE_X86:
			target_setup_x86_abi(target);
			break;
		case ARCH_TYPE_X86_64:
			target_setup_x64_abi(target);
			if (compiler.platform.os == OS_TYPE_WIN32)
			{
				compiler.platform.abi = ABI_WIN64;
				break;
			}
			compiler.platform.abi = ABI_X64;
			break;
		case ARCH_TYPE_UNKNOWN:
			compiler.platform.abi = ABI_UNKNOWN;
			break;
	}
	compiler.platform.align_max_vector = os_arch_max_alignment_of_vector(compiler.platform.os,
																	   compiler.platform.arch,
																	   compiler.platform.environment_type,
																	   compiler.platform.arm.variant,
																	   &compiler.platform.x64.features);
	compiler.platform.align_max_tls = os_arch_max_alignment_of_tls(compiler.platform.os,
																 compiler.platform.arch,
																 compiler.platform.environment_type);
	compiler.platform.reloc_model = arch_os_reloc_default(compiler.platform.arch,
														  compiler.platform.os,
														  compiler.platform.environment_type,
														  compiler.build.type != TARGET_TYPE_EXECUTABLE);
	compiler.platform.pic_required = arch_os_pic_default_forced(compiler.platform.arch, compiler.platform.os);
	// Override PIC, but only if the platform does not require PIC
	if (target->reloc_model != RELOC_DEFAULT
		&& (target->reloc_model != RELOC_NONE || !compiler.platform.pic_required))
	{
		compiler.platform.reloc_model = target->reloc_model;
	}

	if (compiler.platform.os == OS_TYPE_IOS)
	{
		WARNING("iOS not properly supported yet.");
	}
	if (compiler.platform.os == OS_TYPE_MACOSX)
	{
		if (!compiler.build.macos.sysroot) compiler.build.macos.sysroot = macos_sysroot();
		const char *sysroot = compiler.build.macos.sysroot ? compiler.build.macos.sysroot : macos_sysroot();

		compiler.build.macos.sdk = NULL;
		if (sysroot)
		{
			INFO_LOG("Macos SDK: %s", sysroot);
			compiler.build.macos.sdk = macos_sysroot_sdk_information(sysroot);
			if (compiler.platform.arch == ARCH_TYPE_AARCH64)
			{
				if (compiler.build.macos.sdk->macos_min_deploy_target.major < 11)
				{
					compiler.build.macos.sdk->macos_min_deploy_target = (Version) { 11, 0 };
				}
				if (compiler.build.macos.sdk->macos_deploy_target.major < 11)
				{
					compiler.build.macos.sdk->macos_deploy_target = (Version) { 11, 0 };
				}
			}
		}
		compiler.platform.target_triple = strdup(llvm_macos_target_triple(compiler.platform.target_triple));

	}
	ASSERT(compiler.platform.reloc_model != RELOC_DEFAULT);

		// TODO remove
	type_setup(&compiler.platform);


}
