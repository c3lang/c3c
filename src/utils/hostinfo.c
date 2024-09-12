#include "../compiler/compiler_internal.h"

#include "hostinfo.h"
#include <stdlib.h>

static int is_le(void)
{
    unsigned int i = 1;
    char *c;
    c = (char *) &i;
    return (*c == 1);
}

ArchType hostinfo_arch_type(void)
{
#if defined(__x86_64__) || defined(_M_X64)
    return ARCH_TYPE_X86_64;
#elif defined(i386) || defined(__i386__) || defined(__i386) || defined(_M_IX86)
    return ARCH_TYPE_X86;
#elif (defined(__arm__) && !defined(__thumb__)) || (defined(__TARGET_ARCH_ARM) && !defined(__TARGET_ARCH_THUMB)) || defined(__ARM) || defined(_M_ARM) || defined(_M_ARM_T) || defined(__ARM_ARCH)
    return is_le() ? ARCH_TYPE_ARM : ARCH_TYPE_ARMB;
#elif defined(__thumb__) || defined(__TARGET_ARCH_THUMB) || defined(__ARM) || defined(_M_ARM) || defined(_M_ARM_T) || defined(__ARM_ARCH)
    return is_le() ? ARCH_TYPE_THUMB : ARCH_TYPE_THUMBEB;
#elif defined(__aarch64__) || defined(_M_ARM64)
    return is_le() ? ARCH_TYPE_AARCH64 : ARCH_TYPE_AARCH64_BE;
#elif defined(mips) || defined(__mips__) || defined(__mips)
    return ARCH_UNSUPPORTED;
#elif defined(__sh__)
    return ARCH_UNSUPPORTED;
#elif defined(__riscv) && defined(__riscv32)
    return ARCH_TYPE_RISCV32;
#elif defined(__riscv) && defined(__riscv64)
    return ARCH_TYPE_RISCV64;
#elif defined(__PPC64__) || defined(__ppc64__) || defined(_ARCH_PPC64) || defined(__powerpc64__)
    return is_le() ? ARCH_TYPE_PPC64LE : ARCH_TYPE_PPC64;
#elif defined(__powerpc) || defined(__powerpc__) || defined(__POWERPC__) || defined(__ppc__) || defined(__PPC__) || defined(_ARCH_PPC)
    return ARCH_TYPE_PPC;
#elif defined(__sparc__) || defined(__sparc)
    return ARCH_UNSUPPORTED 
#else
    return ARCH_TYPE_UNKNOWN;
#endif
}

static const char *llvm_arch_name(ArchType ty)
{
    switch (ty)
	{
        case ARCH_TYPE_X86_64: return "x86_64";
        case ARCH_TYPE_X86: return "x86";
        case ARCH_TYPE_ARM: return "arm";
        case ARCH_TYPE_ARMB: return "armbe";
        case ARCH_TYPE_AARCH64: return "aarch64";
        case ARCH_TYPE_AARCH64_BE: return "aarch64be";
        case ARCH_TYPE_THUMB: return "thumb";
        case ARCH_TYPE_THUMBEB: return "thumbeb";
        case ARCH_TYPE_WASM64: return "wasm64";
        case ARCH_TYPE_WASM32: return "wasm32";
        case ARCH_TYPE_XTENSA: return "xtensa";
        case ARCH_TYPE_PPC: return "ppc";
        case ARCH_TYPE_PPC64: return "ppc64";
        case ARCH_TYPE_PPC64LE: return "ppc64le";
        case ARCH_TYPE_RISCV32: return "riscv32";
        case ARCH_TYPE_RISCV64: return "riscv64";
		case ARCH_UNSUPPORTED:
        case ARCH_TYPE_UNKNOWN: return "unknown";
    }
	UNREACHABLE
}

void hostinfo_x86_features(X86Features *cpu_features)
{
#if defined(__x86_64__) || defined(_M_X64)
    // TODO
#endif 
}

EnvironmentType hostinfo_env_type(void)
{
    return ENV_TYPE_UNKNOWN;
}

OsType hostinfo_os_type(void)
{
#if __APPLE__
	return OS_TYPE_MACOSX;
#else
    if (system("freebsd-version -k") == 0) return OS_TYPE_FREE_BSD;
    if (system("uname -r") == 0) return OS_TYPE_LINUX;
    if (system("cd C:/Windows") == 0) return OS_TYPE_WIN32;
    return OS_TYPE_UNKNOWN;
#endif
}

static const char *llvm_os_name(OsType os)
{
	switch (os)
	{
		case OS_TYPE_FREE_BSD: return "freebsd";
		case OS_TYPE_LINUX: return "linux";
		case OS_TYPE_WIN32: return "win32";
		case OS_TYPE_MACOSX: return "darwin";
		default: return "unknown";
	}
}

VendorType hostinfo_vendor_type(void)
{
    return VENDOR_UNKNOWN;
}

static char triple[128];
static int  triple_init = 0;

const char * hostinfo_default_triple(void)
{
    if (!triple_init)
	{
        sprintf(triple, "%s-unknown-unknown-%s",
                llvm_arch_name(hostinfo_arch_type()),
                llvm_os_name(hostinfo_os_type()));
        triple_init = 1;
    }

    return triple;
}

const char *hostinfo_x86_cpu_name(void)
{
    return "x86"; 
}
