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


// -------------------------------- Lcg128_64 --------------------------------

def Lcg128_64 = distinct uint128;

fn void Lcg128_64.seed(&lcg, char[16] seed)
{
    *lcg = bitcast(seed, Lcg128_64);
}

fn ulong Lcg128_64.next(&lcg)
{
    uint128* s = (uint128*)lcg;
    ulong result = (ulong)(*s >> 64);
    *s = *s * MUL_LCG128 + ODD_PHI128;
    return result;
}


// -------------------------------- Lcg64_32 --------------------------------

def Lcg64_32 = distinct ulong;

fn void Lcg64_32.seed(&lcg, char[8] seed)
{
    *lcg = bitcast(seed, Lcg64_32);
}

fn uint Lcg64_32.next(&lcg)
{
    ulong* s = (ulong*)lcg;
    uint result = (uint)(*s >> 32);
    *s = *s * MUL_LCG64 + ODD_PHI64;
    return result;
}


// -------------------------------- Lcg32_16 --------------------------------

def Lcg32_16 = distinct uint;

fn void Lcg32_16.seed(&lcg, char[4] seed)
{
    *lcg = bitcast(seed, Lcg32_16);
}

fn ushort Lcg32_16.next(&lcg)
{
    uint* s = (uint*)lcg;
    ushort result = (ushort)(*s >> 16);
    *s = *s * MUL_LCG32 + ODD_PHI32;
    return result;
}


// -------------------------------- Lcg16_8 --------------------------------

def Lcg16_8 = distinct ushort;

fn void Lcg16_8.seed(&lcg, char[2] seed)
{
    *lcg = bitcast(seed, Lcg16_8);
}

fn char Lcg16_8.next(&lcg)
{
    ushort* s = (ushort*)lcg;
    char result = (char)(*s >> 8);
    *s = *s * MUL_LCG16 + ODD_PHI16;
    return result;
}
