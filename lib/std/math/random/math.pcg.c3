module std::math;

// Move ODD_PHI into a shared module
const ODD_PHI128 @local = 0x9e3779b97f4a7c15f39cc0605cedc835;
const ODD_PHI64 @local = 0x9e3779b97f4a7c15;
const ODD_PHI32 @local = 0x9e3779b9;
const ODD_PHI16 @local = 0x9e37;

const MUL_LCG128 @local = 0xdb36357734e34abb0050d0761fcdfc15;
const MUL_LCG64 @local = 0xd1342543de82ef95;
const MUL_LCG32 @local = 0x915f77f5;
const MUL_LCG16 @local = 0x915d; // TODO: Find good constant


// -------------------------------- Pcg128_64 --------------------------------

def Pcg128_64 = distinct uint128;

fn void Pcg128_64.seed(Pcg128_64* pcg, char[16] seed)
{
    *pcg = bitcast(seed, Pcg128_64);
}

fn ulong Pcg128_64.next(Pcg128_64* pcg)
{
    const ROT_SHIFT = 64 - 6;
    uint128* s = (uint128*)pcg;
    uint128 xor = *s ^ *s >> ((128 - ROT_SHIFT) / 2);
    char rot = (char)(*s >> (128 - 6));
    *s = *s * MUL_LCG128 + ODD_PHI128;
    return ((ulong)(xor >> ROT_SHIFT)).rotr(rot);
}


// -------------------------------- Pcg64_32 --------------------------------

def Pcg64_32 = distinct ulong;

fn void Pcg64_32.seed(Pcg64_32* pcg, char[8] seed)
{
    *pcg = bitcast(seed, Pcg64_32);
}

fn uint Pcg64_32.next(Pcg64_32* pcg)
{
    const ROT_SHIFT = 32 - 5;
    ulong* s = (ulong*)pcg;
    ulong xor = *s ^ *s >> ((64 - ROT_SHIFT) / 2);
    char rot = (char)(*s >> (64 - 5));
    *s = *s * MUL_LCG64 + ODD_PHI64;
    return ((uint)(xor >> ROT_SHIFT)).rotr(rot);
}


// -------------------------------- Pcg32_16 --------------------------------

def Pcg32_16 = distinct uint;

fn void Pcg32_16.seed(Pcg32_16* pcg, char[4] seed)
{
    *pcg = bitcast(seed, Pcg32_16);
}

fn ushort Pcg32_16.next(Pcg32_16* pcg)
{
    const ROT_SHIFT = 16 - 4;
    uint* s = (uint*)pcg;
    uint xor = *s ^ *s >> ((32 - ROT_SHIFT) / 2);
    char rot = (char)(*s >> (32 - 4));
    *s = *s * MUL_LCG32 + ODD_PHI32;
    return ((ushort)(xor >> ROT_SHIFT)).rotr(rot);
}


// -------------------------------- Pcg16_8 --------------------------------

def Pcg16_8 = distinct ushort;

fn void Pcg16_8.seed(Pcg16_8* pcg, char[2] seed)
{
    *pcg = bitcast(seed, Pcg16_8);
}

fn char Pcg16_8.next(Pcg16_8* pcg)
{
    const ROT_SHIFT = 8 - 3;
    ushort* s = (ushort*)pcg;
    ushort xor = *s ^ *s >> ((16 - ROT_SHIFT) / 2);
    char rot = (char)(*s >> (16 - 3));
    *s = *s * MUL_LCG16 + ODD_PHI16;
    return ((char)(xor >> ROT_SHIFT)).rotr(rot);
}
