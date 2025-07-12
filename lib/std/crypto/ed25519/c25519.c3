/*
 Operations on the twisted Edwards curve -x^2+y^2=1-121665/121666*x^2*y^2 over the prime field F_(2^255-19) (edwards25519)
 The set of F_(2^255-19)-rational curve points is a group of order 2^3*(2^252 + 0x14def9dea2f79cd65812631a5cf5d3ed)
*/

module std::crypto::ed25519::c25519;
import std::crypto::ed25519::f25519;

// Affine coordinates.
struct Point {
    Int x;
    Int y;
}

// Projective coordinates.
struct Projection {
    Int x;
    Int y;
    Int t;
    Int z;
}

// Neutral.
const Projection NEUTRAL = {
    f25519::ZERO,
    f25519::ONE,
    f25519::ZERO,
    f25519::ONE
};

<*
 Convert affine to projective coordinates.

 @param [&in] p
*>
fn Projection project(Point* p) => {p.x, p.y, p.x.mul(&p.y), f25519::ONE};

<*
 Convert projective to affine coordinates.

 @param [&in] p
*>
fn Point unproject(Projection* p) {
    Point r @noinit;

    Int inv = p.z.inv();
    r.x = p.x.mul(&inv);
    r.y = p.y.mul(&inv);

    r.x.normalize();
    r.y.normalize();

    return r;
}

// d parameter for edwards25519 : -121665/121666
const Int D = {
    0xa3, 0x78, 0x59, 0x13, 0xca, 0x4d, 0xeb, 0x75,
    0xab, 0xd8, 0x41, 0x41, 0x4d, 0x0a, 0x70, 0x00,
    0x98, 0xe8, 0x79, 0x77, 0x79, 0x40, 0xc7, 0x8c,
    0x73, 0xfe, 0x6f, 0x2b, 0xee, 0x6c, 0x03, 0x52
};

<*
 Compress a point.

 @param [&in] p
*>
fn Int pack(Point* p) {
    Point r = *p;

    r.x.normalize();
    r.y.normalize();

    r.y[^1] |= (r.x[0] & 1) << 7;

    return r.y;
}

struct Unpacking {
    Point point;
    char on_curve; // Non-zero if true.
}

<*
 Uncompress a point. Check if it is on the curve.

 @param [&in] encoding
*>
fn Unpacking unpack_on_curve(Int* encoding) {
    Point p @noinit;

    char parity = (*encoding)[^1] >> 7;

    p.y = *encoding;
    p.y[^1] &= 0b01111111;

    Int y2 = p.y.mul(&p.y);
    Int x2 = D.mul(&y2).add(&f25519::ONE).inv().mul(&&y2.sub(&f25519::ONE));

    Int x = x2.sqrt();

    p.x = f25519::select(&x, &&x.neg(), (x[0] ^ parity) & 1);

    Int _x2 = p.x.mul(&p.x);

    x2.normalize();
    _x2.normalize();

    return {p, f25519::eq(&x2, &_x2)};
}

// 2*d
const Int DD = {
    0x59, 0xf1, 0xb2, 0x26, 0x94, 0x9b, 0xd6, 0xeb,
    0x56, 0xb1, 0x83, 0x82, 0x9a, 0x14, 0xe0, 0x00,
    0x30, 0xd1, 0xf3, 0xee, 0xf2, 0x80, 0x8e, 0x19,
    0xe7, 0xfc, 0xdf, 0x56, 0xdc, 0xd9, 0x06, 0x24
};

<*
 Addition.

 @param [&in] s
*>
fn Projection Projection.add(&s, Projection* p) {
    Projection r @noinit;

    Int a = s.y.sub(&s.x).mul(&&p.y.sub(&p.x));
    Int b = s.y.add(&s.x).mul(&&p.y.add(&p.x));
    Int c = s.t.mul(&DD).mul(&p.t);
    Int d = s.z.mul(&p.z).mul_s(2);
    Int e = b.sub(&a);
    Int f = d.sub(&c);
    Int g = d.add(&c);
    Int h = b.add(&a);

    r.x = e.mul(&f);
    r.y = g.mul(&h);
    r.t = e.mul(&h);
    r.z = f.mul(&g);

    return r;
}

<*
 Double a point.

 @param [&in] s
*>
fn Projection Projection.twice(&s) {
    Projection r @noinit;

    Int a = s.x.mul(&s.x);
    Int b = s.y.mul(&s.y);
    Int c = s.z.mul(&s.z).mul_s(2);
    Int d = s.x.add(&s.y);
    Int e = d.mul(&d).sub(&a).sub(&b);
    Int g = b.sub(&a);
    Int f = g.sub(&c);
    Int h = b.neg().sub(&a);

    r.x = e.mul(&f);
    r.y = g.mul(&h);
    r.t = e.mul(&h);
    r.z = f.mul(&g);

    return r;
}

<*
 Variable base scalar multiplication.

 @param [&in] s
 @param [in] n
*>
fn Projection Projection.mul(&s, char[] n) {
    Projection r = NEUTRAL;

    for (isz i = n.len << 3 - 1; i >= 0; i--) {
        r = r.twice();

        Projection t = r.add(s);

        char bit = n[i >> 3] >> (i & 7) & 1;
        r.x = f25519::select(&r.x, &t.x, bit);
        r.y = f25519::select(&r.y, &t.y, bit);
        r.z = f25519::select(&r.z, &t.z, bit);
        r.t = f25519::select(&r.t, &t.t, bit);
    }

    return r;
}
