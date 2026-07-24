/* Enable support for concatenated .xz files. */
#define XZ_DEC_CONCATENATED

/* Enable all supported integrity check types. CRC32 is always enabled. */
#define XZ_USE_CRC64
#define XZ_USE_SHA256
#define XZ_DEC_ANY_CHECK

/* Enable all BCJ filters. */
#define XZ_DEC_X86
#define XZ_DEC_ARM
#define XZ_DEC_ARMTHUMB
#define XZ_DEC_ARM64
#define XZ_DEC_RISCV
#define XZ_DEC_POWERPC
#define XZ_DEC_IA64
#define XZ_DEC_SPARC