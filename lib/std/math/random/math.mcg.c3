module std::math;

const MUL_MCG128 @local = 0x9e3779b97f4a7c15f39cc0605cedc835;
const MUL_MCG64 @local = 0xf1357aea2e62a9c5;
const MUL_MCG32 @local = 0x93d765dd;
const MUL_MCG16 @local = 0x93d5; // TODO: Find good constant

// -------------------------------- Mcg128_64 --------------------------------

def Mcg128_64 = distinct uint128;

fn void Mcg128_64.seed(&mcg, char[16] seed)
{
    *mcg = bitcast(seed, Mcg128_64) | 1;
}

fn ulong Mcg128_64.next(&mcg)
{
    uint128* s = (uint128*)mcg;
    ulong result = (ulong)(*s >> 64);
    *s *= MUL_MCG128;
    return result;
}


// -------------------------------- Mcg64_32 --------------------------------

def Mcg64_32 = distinct ulong;

fn void Mcg64_32.seed(&mcg, char[8] seed)
{
    *mcg = bitcast(seed, Mcg64_32) | 1;
}

fn uint Mcg64_32.next(&mcg)
{
    ulong* s = (ulong*)mcg;
    uint result = (uint)(*s >> 32);
    *s *= MUL_MCG64;
    return result;
}


// -------------------------------- Mcg32_16 --------------------------------

def Mcg32_16 = distinct uint;

fn void Mcg32_16.seed(&mcg, char[4] seed)
{
    *mcg = bitcast(seed, Mcg32_16) | 1;
}

fn ushort Mcg32_16.next(&mcg)
{
    uint* s = (uint*)mcg;
    ushort result = (ushort)(*s >> 16);
    *s *= MUL_MCG32;
    return result;
}


// -------------------------------- Mcg16_8 --------------------------------

def Mcg16_8 = distinct ushort;

fn void Mcg16_8.seed(&mcg, char[2] seed)
{
    *mcg = bitcast(seed, Mcg16_8) | 1;
}

fn char Mcg16_8.next(&mcg)
{
    ushort* s = (ushort*)mcg;
    char result = (char)(*s >> 8);
    *s *= MUL_MCG16;
    return result;
}
