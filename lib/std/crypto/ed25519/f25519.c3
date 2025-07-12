/*
 Modular arithmetic over the prime field F_(2^255-19)
*/

module std::crypto::ed25519::f25519 @private;

typedef Int = inline char[32];

const Int ZERO = {};
const Int ONE = {[0] = 1};

<*
 Reduce an element with carry to at most 2^255 + 18 (32 bytes)

 @param [&inout] s
*>
fn void Int.reduce_carry(&s, uint carry) { // Reduce using 2^255 = 19 mod p
    (*s)[^1] &= 0b01111111;

    carry *= 19;

    for (usz i; i < Int.len; i++) {
        carry += (*s)[i];
        (*s)[i] = (char)carry;
        carry >>= 8;
    }
}

<*
 Reduce an element to at most 2^255-19

 @param [&inout] s
*>
fn void Int.normalize(&s) {
    s.reduce_carry((*s)[^1] >> 7);

    Int sub @noinit;
    ushort c = 19;
    for (usz i; i + 1 < Int.len; i++) { // Substract p
        c += (*s)[i];
        sub[i] = (char)c;
        c >>= 8;
    }
    c += (*s)[^1] - 0b10000000;
    sub[^1] = (char)c;

    *s = select(&sub, s, (char)(c >> 15));
}

<*
 Constant-time equality comparison. Return is non-zero if true.

 @param [&in] a
 @param [&in] b
*>
fn char eq(Int* a, Int* b) {
    char e;
    for (usz i; i < Int.len; i++) e |= (*a)[i] ^ (*b)[i];

    e |= (e >> 4);
    e |= (e >> 2);
    e |= (e >> 1);

    return e ^ 1;
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
        c >>= 8;
        c += (*s)[i] + (*n)[i];
        r[i] = (char)c;
    }

    r.reduce_carry(c >> 7);

    return r;
}

<*
 Substraction.

 @param [&in] s
 @param [&in] n
*>
fn Int Int.sub(&s, Int* n) { // Compute s+2*p-n instead of s-n to avoid underflow.
    Int r @noinit;

    uint c = (char)~(2 * 19 - 1);
    for (usz i; i + 1 < Int.len; i++) {
        c += 0b11111111_00000000 + (*s)[i] - (*n)[i];
        r[i] = (char)c;
        c >>= 8;
    }
    c += (*s)[^1] - (*n)[^1];
    r[^1] = (char)c;

    r.reduce_carry(c >> 7);

    return r;
}

<*
 Negation.

 @param [&in] s
*>
fn Int Int.neg(&s) { // Compute 2*p-s insead of -s to avoid underflow.
    Int r @noinit;

    uint c = (char)~(2 * 19 - 1);
    for (usz i; i + 1 < Int.len; i++) {
        c += 0b11111111_00000000 - (*s)[i];
        r[i] = (char)c;
        c >>= 8;
    }
    c -= (*s)[^1];
    r[^1] = (char)c;

    r.reduce_carry(c >> 7);

    return r;
}

<*
 Multiplication.

 @param [&in] s
 @param [&in] n
*>
fn Int Int.mul(&s, Int* n) {
    Int r @noinit;

    uint c;
    for (usz i; i < Int.len; i++) {
        c >>= 8;
        for (usz j; j <= i; j++) c += (*s)[j] * (*n)[i - j];
        for (usz j = i + 1; j < Int.len; j++) c += (*s)[j] * (*n)[^j - i] * 2 * 19; // Reduce using 2^256 = 2*19 mod p
        r[i] = (char)c;
    }

    r.reduce_carry(c >> 7);

    return r;
}

<*
 Multiplication by a small element.

 @param [&in] s
*>
fn Int Int.mul_s(&s, uint n) {
    Int r @noinit;

    uint c;
    for (usz i; i < Int.len; i++) {
        c >>= 8;
        c += (*s)[i] * n;
        r[i] = (char)c;
    }

    r.reduce_carry(c >> 7);

    return r;
}

<*
 Inverse an element.

 @param [&in] s
*>
fn Int Int.inv(&s) { //Compute s^(p-2)
    Int r = *s;

    for (usz i; i < 255 - 1 - 5; i++) r = r.mul(&r).mul(s);

    r = r.mul(&r);

    r = r.mul(&r).mul(s);

    r = r.mul(&r);

    r = r.mul(&r).mul(s);

    r = r.mul(&r).mul(s);

    return r;
}

<*
 Raise an element to the power of 2^252-3

 @param [&in] s
*>
fn Int Int.pow_2523(&s) @local {
    Int r = *s;

    for (usz i; i < 252 - 1 - 2; i++) r = r.mul(&r).mul(s);

    r = r.mul(&r);

    r = r.mul(&r).mul(s);

    return r;
}

<*
 Compute the square root of an element.

 @param [&in] s
*>
fn Int Int.sqrt(&s) {
    Int twice = s.mul_s(2); Int pow = twice.pow_2523();

    return twice.mul(&pow).mul(&pow).sub(&ONE).mul(s).mul(&pow);
}
