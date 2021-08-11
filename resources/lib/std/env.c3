module std::env;

enum CompilerOptLevel
{
    O0,
    O1,
    O2,
    O3
}

const CompilerOptLevel COMPILER_OPT_LEVEL = (CompilerOptLevel)(${COMPILER_OPT_LEVEL});
const bool BIG_ENDIAN = ${PLATFORM_BIG_ENDIAN};
const bool I128_SUPPORT = ${PLATFORM_I128_SUPPORTED};
const bool COMPILER_SAFE_MODE = ${COMPILER_SAFE_MODE};
