/*
 Ed25519 Digital Signature Algorithm
*/

module std::crypto::ed25519;
import std::hash::sha512;

alias Ed25519PrivateKey = char[32];
alias Ed25519PublicKey = char[Ed25519PrivateKey.len];
alias Ed25519Signature = char[2 * Ed25519PublicKey.len];

<*
 Generate a public key from a private key.

 @param [in] private_key : "32 bytes of cryptographically secure random data"
 @require private_key.len == Ed25519PrivateKey.len
*>
fn Ed25519PublicKey public_keygen(char[] private_key)
{
	return pack(&&unproject(&&(BASE * expand_private_key(private_key)[:FBaseInt.len])));
}

<*
 Sign a message.

 @param [in] message
 @param [in] private_key
 @param [in] public_key
 @require private_key.len == Ed25519PrivateKey.len
 @require public_key.len == Ed25519PublicKey.len
*>
fn Ed25519Signature sign(char[] message, char[] private_key, char[] public_key)
{
	Ed25519Signature r @noinit;

	char[*] exp = expand_private_key(private_key);

	Sha512 sha @noinit;
	sha.init();

	sha.update(exp[FBaseInt.len..]);
	sha.update(message);

	FBaseInt k = from_bytes(&&sha.final());

	r[:F25519Int.len] = pack(&&unproject(&&(BASE * k[..])))[..];

	sha.init();

	sha.update(r[:F25519Int.len]);
	sha.update(public_key);
	sha.update(message);

	FBaseInt z = from_bytes(&&sha.final());
	FBaseInt e = from_bytes(exp[:FBaseInt.len]);

	r[F25519Int.len..] = (z * e + k)[..];

	return r;
}

<*
 Verify the signature of a message.

 @param [in] message
 @param [in] signature
 @param [in] public_key
 @require signature.len == Ed25519Signature.len
 @require public_key.len == Ed25519PublicKey.len
*>
fn bool verify(char[] message, char[] signature, char[] public_key)
{
	char ok = 1;

	F25519Int lhs = pack(&&unproject(&&(BASE * signature[F25519Int.len..])));

	Unpacking unp_p = unpack_on_curve((F25519Int*)public_key);
	Projection p = project(&unp_p.point);
	ok &= unp_p.on_curve;

	Sha512 sha @noinit;
	sha.init();

	sha.update(signature[:F25519Int.len]);
	sha.update(public_key);
	sha.update(message);

	FBaseInt z = from_bytes(&&sha.final());

	p = p * z[..];

	Unpacking unp_q = unpack_on_curve((F25519Int*)signature[:F25519Int.len]);
	Projection q = project(&unp_q.point);
	ok &= unp_q.on_curve;

	p = p + q;

	F25519Int rhs = pack(&&unproject(&p));

	return (bool)(ok & eq(&lhs, &rhs));
}

// Base point for Ed25519. Generate a subgroup of order 2^252+0x14def9dea2f79cd65812631a5cf5d3ed
const Projection BASE @private =
{
	x"1ad5258f602d56c9 b2a7259560c72c69 5cdcd6fd31e2a4c0 fe536ecdd3366921",
	x"5866666666666666 6666666666666666 6666666666666666 6666666666666666",
	x"a3ddb7a5b38ade6d f5525177809ff020 7de3ab648e4eea66 65768bd70f5f8767",
	ONE
};

<*
 Compute the pruned SHA-512 hash of a private key.

 @param [in] private_key
 @require private_key.len == Ed25519PrivateKey.len
*>
fn char[sha512::HASH_SIZE] expand_private_key(char[] private_key) @local
{
	char[*] r = sha512::hash(private_key);

	r[0] &= 0b11111000;
	r[FBaseInt.len - 1] &= 0b01111111;
	r[FBaseInt.len - 1] |= 0b01000000;

	return r;
}



/*
 Operations on the twisted Edwards curve -x^2+y^2=1-121665/121666*x^2*y^2 over the prime field F_(2^255-19) (edwards25519)
 The set of F_(2^255-19)-rational curve points is a group of order 2^3*(2^252+0x14def9dea2f79cd65812631a5cf5d3ed)
*/

module std::crypto::ed25519 @private;

// Affine coordinates.
struct Point
{
	F25519Int x;
	F25519Int y;
}

// Projective coordinates.
struct Projection
{
	F25519Int x;
	F25519Int y;
	F25519Int t;
	F25519Int z;
}

// Neutral.
const Projection NEUTRAL =
{
	ZERO,
	ONE,
	ZERO,
	ONE
};

<*
 Convert affine to projective coordinates.

 @param [&in] p
*>
fn Projection project(Point* p) => { p.x, p.y, p.x * p.y, ONE };

<*
 Convert projective to affine coordinates.

 @param [&in] p
*>
fn Point unproject(Projection* p)
{
	Point r @noinit;

	F25519Int inv = p.z.inv();
	r.x = p.x * inv;
	r.y = p.y * inv;

	r.x.normalize();
	r.y.normalize();

	return r;
}

// d parameter for edwards25519 : -121665/121666
const F25519Int D = x"a3785913ca4deb75 abd841414d0a7000 98e879777940c78c 73fe6f2bee6c0352";

// 2*d
const F25519Int DD = x"59f1b226949bd6eb 56b183829a14e000 30d1f3eef2808e19 e7fcdf56dcd90624";

<*
 Compress a point.

 @param [&in] p
*>
fn F25519Int pack(Point* p)
{
	Point r = *p;

	r.x.normalize();
	r.y.normalize();

	r.y[^1] |= (r.x[0] & 1) << 7;

	return r.y;
}

struct Unpacking
{
	Point point;
	<* Non-zero if true. *>
	char on_curve;
}

<*
 Uncompress a point. Check if it is on the curve.

 @param [&in] encoding
*>
fn Unpacking unpack_on_curve(F25519Int* encoding)
{
	Point p @noinit;

	char parity = (*encoding)[^1] >> 7;

	p.y = *encoding;
	p.y[^1] &= 0b01111111;

	F25519Int y2 = p.y * p.y;
	F25519Int x2 = (D * y2 + ONE).inv() * (y2 - ONE);

	F25519Int x = x2.sqrt();

	p.x = f25519_select(&x, &&-x, (x[0] ^ parity) & 1);

	F25519Int _x2 = p.x * p.x;

	x2.normalize();
	_x2.normalize();

	return {p, eq(&x2, &_x2)};
}

macro Projection Projection.@add(&s, Projection #p) @operator(+) => s.add(@addr(#p));
<*
 Addition.

 @param [&in] s
*>
fn Projection Projection.add(&s, Projection* p) @operator(+)
{
	Projection r @noinit;

	F25519Int a = (s.y - s.x) * (p.y - p.x);
	F25519Int b = (s.y + s.x) * (p.y + p.x);
	F25519Int c = s.t * DD * p.t;
	F25519Int d = (s.z * p.z).mul_s(2);
	F25519Int e = b - a;
	F25519Int f = d - c;
	F25519Int g = d + c;
	F25519Int h = b + a;

	r.x = e * f;
	r.y = g * h;
	r.t = e * h;
	r.z = f * g;

	return r;
}

<*
 Double a point.

 @param [&in] s
*>
fn Projection Projection.twice(&s)
{
	Projection r @noinit;

	F25519Int a = s.x * s.x;
	F25519Int b = s.y * s.y;
	F25519Int c = (s.z * s.z).mul_s(2);
	F25519Int d = s.x + s.y;
	F25519Int e = d * d - a - b;
	F25519Int g = b - a;
	F25519Int f = g - c;
	F25519Int h = -b - a;

	r.x = e * f;
	r.y = g * h;
	r.t = e * h;
	r.z = f * g;

	return r;
}

<*
 Variable base scalar multiplication.

 @param [&in] s
 @param [in] n
*>
fn Projection Projection.mul(&s, char[] n) @operator(*)
{
	Projection r = NEUTRAL;

	for (isz i = n.len << 3 - 1; i >= 0; i--)
	{
		r = r.twice();

		Projection t = r + s;

		char bit = n[i >> 3] >> (i & 7) & 1;
		r.x = f25519_select(&r.x, &t.x, bit);
		r.y = f25519_select(&r.y, &t.y, bit);
		r.z = f25519_select(&r.z, &t.z, bit);
		r.t = f25519_select(&r.t, &t.t, bit);
	}

	return r;
}



/*
 Modular arithmetic over the prime field F_(2^255-19)
*/

module std::crypto::ed25519 @private;

typedef F25519Int = inline char[32];

const F25519Int ZERO = {};
const F25519Int ONE = {[0] = 1};

<*
 Reduce an element with carry to at most 2^255+18 (32 bytes)

 @param [&inout] s
*>
fn void F25519Int.reduce_carry(&s, uint carry)
{
	// Reduce using 2^255 = 19 mod p
	(*s)[^1] &= 0b01111111;

	carry *= 19;

	foreach (i, &v : s)
	{
		carry += *v;
		*v = (char)carry;
		carry >>= 8;
	}
}

<*
 Reduce an element to at most 2^255-19

 @param [&inout] s
*>
fn void F25519Int.normalize(&s)
{
	s.reduce_carry((*s)[^1] >> 7);

	// Subtract p
	F25519Int sub @noinit;
	ushort c = 19;
	foreach (i, v : (*s)[:^1])
	{
		c += v;
		sub[i] = (char)c;
		c >>= 8;
	}
	c += (*s)[^1] - 0b10000000;
	sub[^1] = (char)c;

	*s = f25519_select(&sub, s, (char)(c >> 15));
}

<*
 Constant-time equality comparison. Return is non-zero if true.

 @param [&in] a
 @param [&in] b
*>
fn char eq(F25519Int* a, F25519Int* b)
{
	char e;
	foreach (i, v : a) e |= v ^ (*b)[i];

	e |= (e >> 4);
	e |= (e >> 2);
	e |= (e >> 1);

	return e ^ 1;
}

<*
 Constant-time conditional selection. Result is undefined if condition is neither 0 nor 1.

 @param [&in] zero : "selected if condition is 0"
 @param [&in] one : "selected if condition is 1"
*>
fn F25519Int f25519_select(F25519Int* zero, F25519Int* one, char condition)
{
	F25519Int r @noinit;

	foreach (i, z : zero) r[i] = z ^ (-condition & ((*one)[i] ^ z));

	return r;
}

macro F25519Int F25519Int.@add(&s, F25519Int #n) @operator(+) => s.add(@addr(#n));

<*
 Addition.

 @param [&in] s
 @param [&in] n
*>
fn F25519Int F25519Int.add(&s, F25519Int* n) @operator(+)
{
	F25519Int r @noinit;

	ushort c;
	foreach (i, v : s)
	{
		c >>= 8;
		c += v + (*n)[i];
		r[i] = (char)c;
	}

	r.reduce_carry(c >> 7);

	return r;
}

macro F25519Int F25519Int.@sub(&s, F25519Int #n) @operator(-) => s.sub(@addr(#n));

<*
 Subtraction.

 @param [&in] s
 @param [&in] n
*>
fn F25519Int F25519Int.sub(&s, F25519Int* n) @operator(-)
{
	// Compute s+2*p-n instead of s-n to avoid underflow.
	F25519Int r @noinit;

	uint c = (char)~(2 * 19 - 1);
	foreach (i, v : (*s)[:^1])
	{
		c += 0b11111111_00000000 + v - (*n)[i];
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
fn F25519Int F25519Int.neg(&s) @operator(-)
{
	// Compute 2*p-s instead of -s to avoid underflow.
	F25519Int r @noinit;

	uint c = (char)~(2 * 19 - 1);
	foreach (i, v : (*s)[:^1])
	{
		c += 0b11111111_00000000 - v;
		r[i] = (char)c;
		c >>= 8;
	}
	c -= (*s)[^1];
	r[^1] = (char)c;

	r.reduce_carry(c >> 7);

	return r;
}

macro F25519Int F25519Int.@mul(&s, F25519Int #n) @operator(*) => s.mul(@addr(#n));

<*
 Multiplication.

 @param [&in] s
 @param [&in] n
*>
fn F25519Int F25519Int.mul(&s, F25519Int* n) @operator(*)
{
	F25519Int r @noinit;

	uint c;
	for (usz i = 0; i < F25519Int.len; i++)
	{
		c >>= 8;
		for (usz j; j <= i; j++) c += (*s)[j] * (*n)[i - j];
		// Reduce using 2^256 = 2*19 mod p
		for (usz j = i + 1; j < F25519Int.len; j++) c += (*s)[j] * (*n)[^j - i] * 2 * 19;
		r[i] = (char)c;
	}

	r.reduce_carry(c >> 7);

	return r;
}

<*
 Multiplication by a small element.

 @param [&in] s
*>
fn F25519Int F25519Int.mul_s(&s, uint n)
{
	F25519Int r @noinit;

	uint c;
	foreach (i, v : s)
	{
		c >>= 8;
		c += v * n;
		r[i] = (char)c;
	}

	r.reduce_carry(c >> 7);

	return r;
}

<*
 Inverse an element.

 @param [&in] s
*>
fn F25519Int F25519Int.inv(&s)
{
	//Compute s^(p-2)
	F25519Int r = *s;

	for (usz i; i < 255 - 1 - 5; i++) r = r * r * s;

	r *= r;
	r = r * r * s;
	r *= r;
	r = r * r * s;
	r = r * r * s;

	return r;
}

<*
 Raise an element to the power of 2^252-3

 @param [&in] s
*>
fn F25519Int F25519Int.pow_2523(&s) @local
{
	F25519Int r = *s;

	for (usz i; i < 252 - 1 - 2; i++) r = r * r * s;

	r *= r;
	r = r * r * s;

	return r;
}

<*
 Compute the square root of an element.

 @param [&in] s
*>
fn F25519Int F25519Int.sqrt(&s)
{
	F25519Int twice = s.mul_s(2);
	F25519Int pow = twice.pow_2523();

	return (twice * pow * pow - ONE) * s * pow;
}



/*
 Modular arithmetic over the prime field F_(2^252+0x14def9dea2f79cd65812631a5cf5d3ed)
*/

module std::crypto::ed25519 @private;
import std::math;

typedef FBaseInt = inline char[32];

// Order of the field : 2^252+0x14def9dea2f79cd65812631a5cf5d3ed
const FBaseInt ORDER = x"edd3f55c1a631258 d69cf7a2def9de14 0000000000000000 0000000000000010";

<*
 Interpret bytes as a normalized element.

 @param [in] bytes
*>
fn FBaseInt from_bytes(char[] bytes)
{
	FBaseInt r;

	usz bitc = min(252 - 1, bytes.len << 3);
	usz bytec = bitc >> 3;
	usz mod = bitc & 7;
	usz rem = bytes.len << 3 - bitc;

	r[:bytec] = bytes[^bytec..];

	if (mod)
	{
		r <<= mod;
		r[0] |= bytes[^bytec + 1] >> (8 - mod);
	}

	for (isz i = rem - 1; i >= 0; i--)
	{
		r <<= 1;
		r[0] |= bytes[i >> 3] >> (i & 7) & 1;
		r = r.sub_l(&ORDER);
	}

	return r;
}

<*
 Constant-time conditional selection. Result is undefined if condition is neither 0 nor 1.

 @param [&in] zero : "selected if condition is 0"
 @param [&in] one : "selected if condition is 1"
*>
fn FBaseInt fbase_select(FBaseInt* zero, FBaseInt* one, char condition)
{
	FBaseInt r @noinit;

	foreach (i, z : zero) r[i] = z ^ (-condition & ((*one)[i] ^ z));

	return r;
}

macro FBaseInt FBaseInt.@add(&s, FBaseInt #n) @operator(+) => s.add(@addr(#n));

<*
 Addition.

 @param [&in] s
 @param [&in] n
*>
fn FBaseInt FBaseInt.add(&s, FBaseInt* n) @operator(+)
{
	FBaseInt r @noinit;

	ushort c;
	foreach (i, v : s)
	{
		c += v + (*n)[i];
		r[i] = (char)c;
		c >>= 8;
	}

	return r.sub_l(&ORDER);
}

<*
 Subtraction if RHS is less than LHS else identity.

 @param [&in] s
 @param [&in] n
*>
fn FBaseInt FBaseInt.sub_l(&s, FBaseInt* n)
{
	FBaseInt sub @noinit;
	ushort c;
	foreach (i, v : s)
	{
		c = v - (*n)[i] - c;
		sub[i] = (char)c;
		c = (c >> 8) & 1;
	}

	return fbase_select(&sub, s, (char)c);
}

<*
 Left shift.

 @param [&in] s
*>
fn FBaseInt FBaseInt.shl(&s, usz n) @operator(<<)
{
	FBaseInt r @noinit;

	ushort c;
	foreach (i, v : s)
	{
		c |= v << n;
		r[i] = (char)c;
		c >>= 8;
	}

	return r;
}

macro FBaseInt FBaseInt.@mul(&s, FBaseInt #n) @operator(*) => s.mul(@addr(#n));
<*
 Multiplication.

 @param [&in] s
 @param [&in] n
*>
fn FBaseInt FBaseInt.mul(&s, FBaseInt* n) @operator(*)
{
	FBaseInt r;

	for (isz i = 252; i >= 0; i--)
	{
		r = (r << 1).sub_l(&ORDER);
		r = fbase_select(&r, &&(r + s), (*n)[i >> 3] >> (i & 7) & 1);
	}

	return r;
}
