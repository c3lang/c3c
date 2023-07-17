module std::math;

// Move ODD_PHI into a shared module
const ODD_PHI128 @local = 0x9e3779b97f4a7c15f39cc0605cedc835;
const ODD_PHI64 @local = 0x9e3779b97f4a7c15;
const ODD_PHI32 @local = 0x9e3779b9;
const ODD_PHI16 @local = 0x9e37;
const ODD_PHI8 @local = 0x9f;


// -------------------------------- Sfc128 --------------------------------

def Sfc128 = distinct uint128[4];

fn void Sfc128.seed(&sfc, char[16 * 4] seed)
{
    *sfc = bitcast(seed, Sfc128);
}

fn uint128 Sfc128.next(&sfc) // TODO: Find good constant
{
    uint128* s = (uint128[4]*)sfc;
    uint128 result = s[0] + s[1] + s[3];
    s[0] = s[1] ^ s[1] >> 11;
    s[1] = s[2] + s[2] << 3;
    s[2] = s[2].rotr(40) + result;
    s[3] += ODD_PHI128;
    return result;
}


// -------------------------------- Sfc64 --------------------------------

def Sfc64 = distinct ulong[4];

fn void Sfc64.seed(&sfc, char[8 * 4] seed)
{
    *sfc = bitcast(seed, Sfc64);
}

fn ulong Sfc64.next(&sfc)
{
    ulong* s = (ulong[4]*)sfc;
    ulong result = s[0] + s[1] + s[3];
    s[0] = s[1] ^ s[1] >> 11;
    s[1] = s[2] + s[2] << 3;
    s[2] = s[2].rotr(40) + result;
    s[3] += ODD_PHI64;
    return result;
}


// -------------------------------- Sfc32 --------------------------------

def Sfc32 = distinct uint[4];

fn void Sfc32.seed(&sfc, char[4 * 4] seed)
{
    *sfc = bitcast(seed, Sfc32);
}

fn uint Sfc32.next(&sfc)
{
    uint* s = (uint[4]*)sfc;
    uint result = s[0] + s[1] + s[3];
    s[0] = s[1] ^ s[1] >> 9;
    s[1] = s[2] + s[2] << 3;
    s[2] = s[2].rotr(11) + result;
    s[3] += ODD_PHI32;
    return result;
}


// -------------------------------- Sfc16 --------------------------------

def Sfc16 = distinct ushort[4];

fn void Sfc16.seed(&sfc, char[2 * 4] seed)
{
    *sfc = bitcast(seed, Sfc16);
}

fn ushort Sfc16.next(&sfc)
{
    ushort* s = (ushort[4]*)sfc;
    ushort result = s[0] + s[1] + s[3];
    s[0] = s[1] ^ s[1] >> 2;
    s[1] = s[2] + s[2] << 3;
    s[2] = s[2].rotr(12) + result;
    s[3] += ODD_PHI16;
    return result;
}


// -------------------------------- Sfc8 --------------------------------

def Sfc8 = distinct char[4];

fn void Sfc8.seed(&sfc, char[1 * 4] seed)
{
    *sfc = bitcast(seed, Sfc8);
}

fn char Sfc8.next(&sfc) // TODO: Find better constants
{
    char* s = (char[4]*)sfc;
    char result = s[0] + s[1] + s[3];
    s[0] = s[1] ^ s[1] >> 1;
    s[1] = s[2] + s[2] << 2;
    s[2] = s[2].rotr(3) + result;
    s[3] += ODD_PHI8;
    return result;
}
