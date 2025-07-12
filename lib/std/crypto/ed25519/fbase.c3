/*
 Modular arithmetic over the prime field F_(2^252+0x14def9dea2f79cd65812631a5cf5d3ed)
*/

module std::crypto::ed25519::fbase @private;
import std::math;

typedef Int = inline char[32];

// Order of the field : 2^252+0x14def9dea2f79cd65812631a5cf5d3ed
const Int ORDER = {
    0xed, 0xd3, 0xf5, 0x5c, 0x1a, 0x63, 0x12, 0x58,
    0xd6, 0x9c, 0xf7, 0xa2, 0xde, 0xf9, 0xde, 0x14,
    0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
    0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x10
};

<*
 Interpret bytes as a normalized element.

 @param [in] bytes
*>
fn Int from_bytes(char[] bytes) {
    Int r;

    usz bitc = min(252 - 1, bytes.len << 3);
    usz bytec = bitc >> 3;
    usz mod = bitc & 7;
    usz rem = bytes.len << 3 - bitc;

    r[:bytec] = bytes[^bytec..];

    if (mod) {
        r = r.shl(mod);
        r[0] |= bytes[^bytec + 1] >> (8 - mod);
    }

    for (isz i = rem - 1; i >= 0; i--) {
        r = r.shl(1);
        r[0] |= bytes[i >> 3] >> (i & 7) & 1;
        r = r.sub_l(&ORDER);
    }

    return r;
}

<*
 Constant-time conditonal selection. Result is undefined if condition is neither 0 nor 1.

 @param [&in] zero : "selected if condition is 0"
 @param [&in] one : "selected if condition is 1"
*>
fn Int select(Int* zero, Int* one, char condition) {
    Int r @noinit;

    for (usz i; i < Int.len; i++) r[i] = (*zero)[i] ^ (-condition & ((*one)[i] ^ (*zero)[i]));

    return r;
}

<*
 Addition.

 @param [&in] s
 @param [&in] n
*>
fn Int Int.add(&s, Int* n) {
    Int r @noinit;

    ushort c;
    for (usz i; i < Int.len; i++) {
        c += (*s)[i] + (*n)[i];
        r[i] = (char)c;
        c >>= 8;
    }

    return r.sub_l(&ORDER);
}

<*
 Substraction if RHS is less than LHS else identity.

 @param [&in] s
 @param [&in] n
*>
fn Int Int.sub_l(&s, Int* n) {
    Int sub @noinit;
    ushort c;
    for (usz i; i < Int.len; i++) {
        c = (*s)[i] - (*n)[i] - c;
        sub[i] = (char)c;
        c = (c >> 8) & 1;
    }

    return select(&sub, s, (char)c);
}

<*
 Left shift.

 @param [&in] s
*>
fn Int Int.shl(&s, usz n) {
    Int r @noinit;

    ushort c;
        for (usz i; i < Int.len; i++) {
        c |= (*s)[i] << n;
        r[i] = (char)c;
        c >>= 8;
    }

    return r;
}

<*
 Multiplication.

 @param [&in] s
 @param [&in] n
*>
fn Int Int.mul(&s, Int* n) {
    Int r;

    for (isz i = 252; i >= 0; i--) {
        r = r.shl(1).sub_l(&ORDER);
        r = select(&r, &&r.add(s), (*n)[i >> 3] >> (i & 7) & 1);
    }

    return r;
}
