module std::core::cpudetect @if(env::X86 || env::X86_64);

struct CpuId
{
	uint eax, ebx, ecx, edx;
}
fn CpuId x86_cpuid(uint eax, uint ecx = 0)
{
	uint edx;
	uint ebx;
	asm
	{
		movl $eax, eax;
		movl $ecx, ecx;
		cpuid;
		movl eax, $eax;
		movl ebx, $ebx;
		movl ecx, $ecx;
		movl edx, $edx;
	}
	return { eax, ebx, ecx, edx };
}

enum X86Feature
{
	ADX,
	AES,
	AMX_AVX512,
	AMX_FP8,
	AMX_MOVRS,
	AMX_TF32,
	AMX_TRANSPOSE,
	AMX_BF16,
	AMX_COMPLEX,
	AMX_FP16,
	AMX_INT8,
	AMX_TILE,
	APXF,
	AVX,
	AVX10_1_256,
	AVX10_1_512,
	AVX10_2_256,
	AVX10_2_512,
	AVX2,
	AVX5124FMAPS,
	AVX5124VNNIW,
	AVX512BF16,
	AVX512BITALG,
	AVX512BW,
	AVX512CD,
	AVX512DQ,
	AVX512ER,
	AVX512F,
	AVX512FP16,
	AVX512IFMA,
	AVX512PF,
	AVX512VBMI,
	AVX512VBMI2,
	AVX512VL,
	AVX512VNNI,
	AVX512VP2INTERSECT,
	AVX512VPOPCNTDQ,
	AVXIFMA,
	AVXNECONVERT,
	AVXVNNI,
	AVXVNNIINT16,
	AVXVNNIINT8,
	BMI,
	BMI2,
	CLDEMOTE,
	CLFLUSHOPT,
	CLWB,
	CLZERO,
	CMOV,
	CMPCCXADD,
	CMPXCHG16B,
	CX8,
	ENQCMD,
	F16C,
	FMA,
	FMA4,
	FSGSBASE,
	FXSR,
	GFNI,
	HRESET,
	INVPCID,
	KL,
	LWP,
	LZCNT,
	MMX,
	MOVBE,
	MOVDIR64B,
	MOVDIRI,
	MOVRS,
	MWAITX,
	PCLMUL,
	PCONFIG,
	PKU,
	POPCNT,
	PREFETCHI,
	PREFETCHWT1,
	PRFCHW,
	PTWRITE,
	RAOINT,
	RDPID,
	RDPRU,
	RDRND,
	RDSEED,
	RTM,
	SAHF,
	SERIALIZE,
	SGX,
	SHA,
	SHA512,
	SHSTK,
	SM3,
	SM4,
	SSE,
	SSE2,
	SSE3,
	SSE4_1,
	SSE4_2,
	SSE4_A,
	SSSE3,
	TBM,
	TSXLDTRK,
	UINTR,
	USERMSR,
	VAES,
	VPCLMULQDQ,
	WAITPKG,
	WBNOINVD,
	WIDEKL,
	X87,
	XOP,
	XSAVE,
	XSAVEC,
	XSAVEOPT,
	XSAVES,
}

uint128 x86_features;

<* Check whether a given CPU feature is set. *>
macro bool has_cpu_flag(X86Feature f) => (cpudetect::x86_features & (1ULL << f.ordinal)) != 0;

fn void add_feature_if_bit(X86Feature feature, uint register, int bit)
{
	if (register & 1U << bit) x86_features |= 1ULL << feature.ordinal;
}

fn void x86_initialize_cpu_features()
{
	uint max_level = x86_cpuid(0).eax;
	CpuId feat = x86_cpuid(1);
	CpuId leaf7 = max_level >= 7 ? x86_cpuid(7) : {};
	CpuId leaf7s1 = leaf7.eax >= 1 ? x86_cpuid(7, 1) : {};
	CpuId ext1 = x86_cpuid(0x80000000).eax >= 0x80000001 ? x86_cpuid(0x80000001) : {};
	CpuId ext8 = x86_cpuid(0x80000000).eax >= 0x80000008 ? x86_cpuid(0x80000008) : {};
	CpuId leaf_d = max_level >= 0xd ? x86_cpuid(0xd, 0x1) : {};
	CpuId leaf_14 = max_level >= 0x14 ? x86_cpuid(0x14) : {};
	CpuId leaf_19 = max_level >= 0x19 ? x86_cpuid(0x19) : {};
	CpuId leaf_24 = max_level >= 0x24 ? x86_cpuid(0x24) : {};
	add_feature_if_bit(ADX, leaf7.ebx, 19);
	add_feature_if_bit(AES, feat.ecx, 25);
	add_feature_if_bit(AMX_BF16, leaf7.edx, 22);
	add_feature_if_bit(AMX_COMPLEX, leaf7s1.edx, 8);
	add_feature_if_bit(AMX_FP16, leaf7s1.eax, 21);
	add_feature_if_bit(AMX_INT8, leaf7.edx, 25);
	add_feature_if_bit(AMX_TILE, leaf7.edx, 24);
	add_feature_if_bit(APXF, leaf7s1.edx, 21);
	add_feature_if_bit(AVX, feat.ecx, 28);
	add_feature_if_bit(AVX10_1_256, leaf7s1.edx, 19);
	add_feature_if_bit(AVX10_1_512, leaf_24.ebx, 18);
	add_feature_if_bit(AVX2, leaf7.ebx, 5);
	add_feature_if_bit(AVX5124FMAPS, leaf7.edx, 3);
	add_feature_if_bit(AVX5124VNNIW, leaf7.edx, 2);
	add_feature_if_bit(AVX512BF16, leaf7s1.eax, 5);
	add_feature_if_bit(AVX512BITALG, leaf7.ecx, 12);
	add_feature_if_bit(AVX512BW, leaf7.ebx, 30);
	add_feature_if_bit(AVX512CD, leaf7.ebx, 28);
	add_feature_if_bit(AVX512DQ, leaf7.ebx, 17);
	add_feature_if_bit(AVX512ER, leaf7.ebx, 27);
	add_feature_if_bit(AVX512F, leaf7.ebx, 16);
	add_feature_if_bit(AVX512FP16, leaf7.edx, 23);
	add_feature_if_bit(AVX512IFMA, leaf7.ebx, 21);
	add_feature_if_bit(AVX512PF, leaf7.ebx, 26);
	add_feature_if_bit(AVX512VBMI, leaf7.ecx, 1);
	add_feature_if_bit(AVX512VBMI2, leaf7.ecx, 6);
	add_feature_if_bit(AVX512VL, leaf7.ebx, 31);
	add_feature_if_bit(AVX512VNNI, leaf7.ecx, 11);
	add_feature_if_bit(AVX512VP2INTERSECT, leaf7.edx, 8);
	add_feature_if_bit(AVX512VPOPCNTDQ, leaf7.ecx, 14);
	add_feature_if_bit(AVXIFMA, leaf7s1.eax, 23);
	add_feature_if_bit(AVXNECONVERT, leaf7s1.edx, 5);
	add_feature_if_bit(AVXVNNI, leaf7s1.eax, 4);
	add_feature_if_bit(AVXVNNIINT16, leaf7s1.edx, 10);
	add_feature_if_bit(AVXVNNIINT8, leaf7s1.edx, 4);
	add_feature_if_bit(BMI, leaf7.ebx, 3);
	add_feature_if_bit(BMI2, leaf7.ebx, 8);
	add_feature_if_bit(CLDEMOTE, leaf7.ecx, 25);
	add_feature_if_bit(CLFLUSHOPT, leaf7.ebx, 23);
	add_feature_if_bit(CLWB, leaf7.ebx, 24);
	add_feature_if_bit(CLZERO, ext8.ecx, 0);
	add_feature_if_bit(CMOV, feat.edx, 15);
	add_feature_if_bit(CMPCCXADD, leaf7s1.eax, 7);
	add_feature_if_bit(CMPXCHG16B, feat.ecx, 12);
	add_feature_if_bit(CX8, feat.edx, 8);
	add_feature_if_bit(ENQCMD, leaf7.ecx, 29);
	add_feature_if_bit(F16C, feat.ecx, 29);
	add_feature_if_bit(FMA, feat.ecx, 12);
	add_feature_if_bit(FMA4, ext1.ecx, 16);
	add_feature_if_bit(FSGSBASE, leaf7.ebx, 0);
	add_feature_if_bit(FXSR, feat.edx, 24);
	add_feature_if_bit(GFNI, leaf7.ecx, 8);
	add_feature_if_bit(HRESET, leaf7s1.eax, 22);
	add_feature_if_bit(INVPCID, leaf7.ebx, 10);
	add_feature_if_bit(KL, leaf7.ecx, 23);
	add_feature_if_bit(LWP, ext1.ecx, 15);
	add_feature_if_bit(LZCNT, ext1.ecx, 5);
	add_feature_if_bit(MMX, feat.edx, 23);
	add_feature_if_bit(MOVBE, feat.ecx, 22);
	add_feature_if_bit(MOVDIR64B, leaf7.ecx, 28);
	add_feature_if_bit(MOVDIRI, leaf7.ecx, 27);
	add_feature_if_bit(MWAITX, ext1.ecx, 29);
	add_feature_if_bit(PCLMUL, feat.ecx, 1);
	add_feature_if_bit(PCONFIG, leaf7.edx, 18);
	add_feature_if_bit(PKU, leaf7.ecx, 4);
	add_feature_if_bit(POPCNT, feat.ecx, 23);
	add_feature_if_bit(PREFETCHI, leaf7s1.edx, 14);
	add_feature_if_bit(PREFETCHWT1, leaf7.ecx, 0);
	add_feature_if_bit(PRFCHW, ext1.ecx, 8);
	add_feature_if_bit(PTWRITE, leaf_14.ebx, 4);
	add_feature_if_bit(RAOINT, leaf7s1.eax, 3);
	add_feature_if_bit(RDPID, leaf7.ecx, 22);
	add_feature_if_bit(RDPRU, ext8.ecx, 4);
	add_feature_if_bit(RDRND, feat.ecx, 30);
	add_feature_if_bit(RDSEED, leaf7.ebx, 18);
	add_feature_if_bit(RTM, leaf7.ebx, 11);
	add_feature_if_bit(SAHF, ext1.ecx, 0);
	add_feature_if_bit(SERIALIZE, leaf7.edx, 14);
	add_feature_if_bit(SGX, leaf7.ebx, 2);
	add_feature_if_bit(SHA, leaf7.ebx, 29);
	add_feature_if_bit(SHA512, leaf7s1.eax, 0);
	add_feature_if_bit(SHSTK, leaf7.ecx, 7);
	add_feature_if_bit(SM3, leaf7s1.eax, 1);
	add_feature_if_bit(SM4, leaf7s1.eax, 2);
	add_feature_if_bit(SSE, feat.edx, 25);
	add_feature_if_bit(SSE2, feat.edx, 26);
	add_feature_if_bit(SSE3, feat.ecx, 0);
	add_feature_if_bit(SSE4_1, feat.ecx, 19);
	add_feature_if_bit(SSE4_2, feat.ecx, 20);
	add_feature_if_bit(SSE4_A, ext1.ecx, 6);
	add_feature_if_bit(SSSE3, feat.ecx, 9);
	add_feature_if_bit(TBM, ext1.ecx, 21);
	add_feature_if_bit(TSXLDTRK, leaf7.edx, 16);
	add_feature_if_bit(UINTR, leaf7.edx, 5);
	add_feature_if_bit(USERMSR, leaf7s1.edx, 15);
	add_feature_if_bit(VAES, leaf7.ecx, 9);
	add_feature_if_bit(VPCLMULQDQ, leaf7.ecx, 10);
	add_feature_if_bit(WAITPKG, leaf7.ecx, 5);
	add_feature_if_bit(WBNOINVD, ext8.ecx, 9);
	add_feature_if_bit(WIDEKL, leaf_19.ebx, 2);
	add_feature_if_bit(X87, feat.edx, 0);
	add_feature_if_bit(XOP, ext1.ecx, 11);
	add_feature_if_bit(XSAVE, feat.ecx, 26);
	add_feature_if_bit(XSAVEC, leaf_d.eax, 1);
	add_feature_if_bit(XSAVEOPT, leaf_d.eax, 0);
	add_feature_if_bit(XSAVES, leaf_d.eax, 3);

}

module std::core::cpudetect @if(env::AARCH64);

enum Aarch64Feature
{
	AES,
	ASIMD,           // Advanced SIMD (NEON) — mandatory on ARMv8-A
	BF16,
	BTI,
	CRC32,
	DIT,
	DOTPROD,
	DPB,             // FEAT_DPB / DCPOP
	DPB2,            // FEAT_DPB2
	EBF16,
	F32MM,
	F64MM,
	FCMA,
	FLAGM,
	FLAGM2,
	FP,              // Floating point — mandatory on ARMv8-A
	FP16,            // Half-precision FP (FPHP)
	FP16FML,         // FEAT_FHM
	FRINTTS,
	I8MM,
	JSCVT,
	LRCPC,
	LRCPC2,
	LRCPC3,
	LSE,             // Large System Extensions (ATOMICS)
	MEMTAG,          // MTE
	MOPS,
	PMULL,
	RPRES,
	RDM,
	RNG,
	SHA1,
	SHA2,
	SHA3,
	SHA512,
	SM3,
	SM4,
	SME,
	SME2,
	SME_F64F64,
	SME_I16I64,
	SSBS,
	SVE,
	SVE2,
	SVE2_AES,
	SVE2_BITPERM,
	SVE2_SHA3,
	SVE2_SM4,
	SVE_BF16,
	SVE_I8MM,
	WFXT,
}

uint128 aarch64_features;

<* Check whether a given CPU feature is set. *>
macro bool has_cpu_flag(Aarch64Feature f) => (cpudetect::aarch64_features & (1ULL << f.ordinal)) != 0;

fn void add_feature_if_bit(Aarch64Feature feature, ulong reg, int bit)
{
	if (reg & 1UL << bit) aarch64_features |= 1ULL << feature.ordinal;
}

fn void aarch64_initialize_cpu_features()
{
	$switch:
		$case env::LINUX:
		$case env::ANDROID:
			ulong hwcap = getauxval(AT_HWCAP);
			ulong hwcap2 = getauxval(AT_HWCAP2);
			// HWCAP — https://raw.githubusercontent.com/torvalds/linux/master/arch/arm64/include/uapi/asm/hwcap.h
			add_feature_if_bit(FP,       hwcap, 0);
			add_feature_if_bit(ASIMD,    hwcap, 1);
			add_feature_if_bit(AES,      hwcap, 3);
			add_feature_if_bit(PMULL,    hwcap, 4);
			add_feature_if_bit(SHA1,     hwcap, 5);
			add_feature_if_bit(SHA2,     hwcap, 6);
			add_feature_if_bit(CRC32,    hwcap, 7);
			add_feature_if_bit(LSE,      hwcap, 8);
			add_feature_if_bit(FP16,     hwcap, 9);
			add_feature_if_bit(RDM,      hwcap, 12);
			add_feature_if_bit(JSCVT,    hwcap, 13);
			add_feature_if_bit(FCMA,     hwcap, 14);
			add_feature_if_bit(LRCPC,    hwcap, 15);
			add_feature_if_bit(DPB,      hwcap, 16);
			add_feature_if_bit(SHA3,     hwcap, 17);
			add_feature_if_bit(SM3,      hwcap, 18);
			add_feature_if_bit(SM4,      hwcap, 19);
			add_feature_if_bit(DOTPROD,  hwcap, 20);
			add_feature_if_bit(SHA512,   hwcap, 21);
			add_feature_if_bit(SVE,      hwcap, 22);
			add_feature_if_bit(FP16FML,  hwcap, 23);
			add_feature_if_bit(DIT,      hwcap, 24);
			add_feature_if_bit(LRCPC2,   hwcap, 26);
			add_feature_if_bit(FLAGM,    hwcap, 27);
			add_feature_if_bit(SSBS,     hwcap, 28);
			// HWCAP2
			add_feature_if_bit(DPB2,         hwcap2, 0);
			add_feature_if_bit(SVE2,         hwcap2, 1);
			add_feature_if_bit(SVE2_AES,     hwcap2, 2);
			add_feature_if_bit(SVE2_BITPERM, hwcap2, 4);
			add_feature_if_bit(SVE2_SHA3,    hwcap2, 5);
			add_feature_if_bit(SVE2_SM4,     hwcap2, 6);
			add_feature_if_bit(FLAGM2,       hwcap2, 7);
			add_feature_if_bit(FRINTTS,      hwcap2, 8);
			add_feature_if_bit(SVE_I8MM,     hwcap2, 9);
			add_feature_if_bit(F32MM,        hwcap2, 10);
			add_feature_if_bit(F64MM,        hwcap2, 11);
			add_feature_if_bit(SVE_BF16,     hwcap2, 12);
			add_feature_if_bit(I8MM,         hwcap2, 13);
			add_feature_if_bit(BF16,         hwcap2, 14);
			add_feature_if_bit(RNG,          hwcap2, 16);
			add_feature_if_bit(BTI,          hwcap2, 17);
			add_feature_if_bit(MEMTAG,       hwcap2, 18);
			add_feature_if_bit(RPRES,        hwcap2, 21);
			add_feature_if_bit(SME,          hwcap2, 23);
			add_feature_if_bit(SME_I16I64,   hwcap2, 24);
			add_feature_if_bit(SME_F64F64,   hwcap2, 25);
			add_feature_if_bit(WFXT,         hwcap2, 31);
			add_feature_if_bit(EBF16,        hwcap2, 32);
			add_feature_if_bit(SME2,         hwcap2, 37);
			add_feature_if_bit(MOPS,         hwcap2, 43);
			add_feature_if_bit(LRCPC3,       hwcap2, 46);
		$case env::WIN32:
			// FP and ASIMD are mandatory on ARMv8-A
			aarch64_features |= 1ULL << Aarch64Feature.FP.ordinal;
			aarch64_features |= 1ULL << Aarch64Feature.ASIMD.ordinal;
			add_feature_if_bit(CRC32,   isProcessorFeaturePresent(33), 0);
			add_feature_if_bit(LSE,     isProcessorFeaturePresent(34), 0);
			add_feature_if_bit(FP16,    isProcessorFeaturePresent(36), 0);
			add_feature_if_bit(DOTPROD, isProcessorFeaturePresent(43), 0);
			add_feature_if_bit(JSCVT,   isProcessorFeaturePresent(44), 0);
			add_feature_if_bit(FCMA,    isProcessorFeaturePresent(45), 0);
			add_feature_if_bit(LRCPC,   isProcessorFeaturePresent(46), 0);
			add_feature_if_bit(BF16,    isProcessorFeaturePresent(48), 0);
			add_feature_if_bit(I8MM,    isProcessorFeaturePresent(49), 0);
			add_feature_if_bit(RNG,     isProcessorFeaturePresent(50), 0);
			add_feature_if_bit(FLAGM,   isProcessorFeaturePresent(52), 0);
			add_feature_if_bit(FLAGM2,  isProcessorFeaturePresent(53), 0);
			add_feature_if_bit(SVE,     isProcessorFeaturePresent(54), 0);
		$case env::DARWIN:
			// Add guaranteed features.
			aarch64_features |= 1ULL << Aarch64Feature.FP.ordinal;
			aarch64_features |= 1ULL << Aarch64Feature.ASIMD.ordinal;
			add_feature_if_sysctl(AES,        "hw.optional.arm.FEAT_AES");
			add_feature_if_sysctl(PMULL,      "hw.optional.arm.FEAT_PMULL");
			add_feature_if_sysctl(SHA1,       "hw.optional.arm.FEAT_SHA1");
			add_feature_if_sysctl(SHA2,       "hw.optional.arm.FEAT_SHA256");
			add_feature_if_sysctl(SHA3,       "hw.optional.arm.FEAT_SHA3");
			add_feature_if_sysctl(SHA512,     "hw.optional.arm.FEAT_SHA512");
			add_feature_if_sysctl(SM3,        "hw.optional.arm.FEAT_SM3");
			add_feature_if_sysctl(SM4,        "hw.optional.arm.FEAT_SM4");
			add_feature_if_sysctl(CRC32,      "hw.optional.arm.FEAT_CRC32");

			add_feature_if_sysctl(FP16FML,    "hw.optional.arm.FEAT_FHM");
			add_feature_if_sysctl(DOTPROD,    "hw.optional.arm.FEAT_DotProd");
			add_feature_if_sysctl(RDM,        "hw.optional.arm.FEAT_RDM");
			add_feature_if_sysctl(LSE,        "hw.optional.arm.FEAT_LSE");
			add_feature_if_sysctl(FRINTTS,    "hw.optional.arm.FEAT_FRINTTS");
			add_feature_if_sysctl(LRCPC,      "hw.optional.arm.FEAT_LRCPC");
			add_feature_if_sysctl(LRCPC2,     "hw.optional.arm.FEAT_LRCPC2");
			add_feature_if_sysctl(FCMA,       "hw.optional.arm.FEAT_FCMA");
			add_feature_if_sysctl(JSCVT,      "hw.optional.arm.FEAT_JSCVT");
			add_feature_if_sysctl(DPB,        "hw.optional.arm.FEAT_DPB");
			add_feature_if_sysctl(DPB2,       "hw.optional.arm.FEAT_DPB2");

			add_feature_if_sysctl(BF16,       "hw.optional.arm.FEAT_BF16");
			add_feature_if_sysctl(EBF16,      "hw.optional.arm.FEAT_EBF16");
			add_feature_if_sysctl(I8MM,       "hw.optional.arm.FEAT_I8MM");
			add_feature_if_sysctl(WFXT,       "hw.optional.arm.FEAT_WFxT");
			add_feature_if_sysctl(RPRES,      "hw.optional.arm.FEAT_RPRES");

			add_feature_if_sysctl(DIT,        "hw.optional.arm.FEAT_DIT");
			add_feature_if_sysctl(FP16,       "hw.optional.arm.FEAT_FP16");
			add_feature_if_sysctl(SSBS,       "hw.optional.arm.FEAT_SSBS");
			add_feature_if_sysctl(BTI,        "hw.optional.arm.FEAT_BTI");
			add_feature_if_sysctl(MEMTAG,     "hw.optional.arm.FEAT_MTE");

			add_feature_if_sysctl(SME_F64F64, "hw.optional.arm.FEAT_SME_F64F64");
			add_feature_if_sysctl(SME_I16I64, "hw.optional.arm.FEAT_SME_I16I64");
			add_feature_if_sysctl(SME,        "hw.optional.arm.FEAT_SME");
			add_feature_if_sysctl(SME2,       "hw.optional.arm.FEAT_SME2");
		$default:
			// Unsupported OS — features remain zero.
	$endswitch
}


module std::core::cpudetect @if(env::AARCH64 && (env::LINUX || env::ANDROID));

const ulong AT_HWCAP = 16;
const ulong AT_HWCAP2 = 26;

extern fn ulong getauxval(ulong type);


module std::core::cpudetect @if(env::AARCH64 && env::WIN32);

extern fn int isProcessorFeaturePresent(ulong feature) @cname("isProcessorFeaturePresent");


module std::core::cpudetect @if(env::AARCH64 && env::DARWIN);

extern fn CInt sysctlbyname(ZString name, void* oldp, usz* oldlenp, void* newp, usz newlen);

fn bool sysctl_bool(ZString name) @local
{
	int value;
	usz size = int::size;
	if (sysctlbyname(name, &value, &size, null, 0) != 0) return false;
	return value != 0;
}

fn void add_feature_if_sysctl(Aarch64Feature feature, ZString name)
{
	if (sysctl_bool(name)) aarch64_features |= 1ULL << feature.ordinal;
}