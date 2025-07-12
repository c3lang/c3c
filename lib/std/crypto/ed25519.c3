/*
 Ed25519 Digital Signature Algorithm
*/

module std::crypto::ed25519;
import std::crypto::ed25519::c25519 @public, std::crypto::ed25519::f25519 @public, std::crypto::ed25519::fbase @public;
import std::hash::sha512;

alias PrivateKey = char[32];
alias PublicKey = char[PrivateKey.len];
alias Signature = char[2 * PublicKey.len];

<*
 Generate a public key from a private key.

 @param [in] private_key : "32 bytes of cryptographically secure random data"
 @require private_key.len == PrivateKey.len
*>
fn PublicKey public_keygen(char[] private_key) => c25519::pack(&&c25519::unproject(&&BASE.mul(expand_private_key(private_key)[:fbase::Int.len])));

<*
 Sign a message.

 @param [in] message
 @param [in] private_key
 @param [in] public_key
 @require private_key.len == PrivateKey.len
 @require public_key.len == PublicKey.len
*>
fn Signature sign(char[] message, char[] private_key, char[] public_key) {
    Signature r @noinit;

    char[*] exp = expand_private_key(private_key);

    Sha512 sha @noinit;
    sha.init();

    sha.update(exp[fbase::Int.len..]);
    sha.update(message);

    fbase::Int k = fbase::from_bytes(&&sha.final());

    r[:f25519::Int.len] = c25519::pack(&&c25519::unproject(&&BASE.mul(k[..])))[..];

    sha.init();

    sha.update(r[:f25519::Int.len]);
    sha.update(public_key);
    sha.update(message);

    fbase::Int z = fbase::from_bytes(&&sha.final());
    fbase::Int e = fbase::from_bytes(exp[:fbase::Int.len]);

    r[f25519::Int.len..] = z.mul(&e).add(&k)[..];

    return r;
}

<*
 Verify the signature of a message.

 @param [in] message
 @param [in] signature
 @param [in] public_key
 @require signature.len == Signature.len
 @require public_key.len == PublicKey.len
*>
fn bool verify(char[] message, char[] signature, char[] public_key) {
    char ok = 1;

    f25519::Int lhs = c25519::pack(&&c25519::unproject(&&BASE.mul(signature[f25519::Int.len..])));

    Unpacking unp_p = c25519::unpack_on_curve((f25519::Int*)public_key);
    Projection p = c25519::project(&unp_p.point);
    ok &= unp_p.on_curve;

    Sha512 sha @noinit;
    sha.init();

    sha.update(signature[:f25519::Int.len]);
    sha.update(public_key);
    sha.update(message);

    fbase::Int z = fbase::from_bytes(&&sha.final());

    p = p.mul(z[..]);

    Unpacking unp_q = c25519::unpack_on_curve((f25519::Int*)signature[:f25519::Int.len]);
    Projection q = c25519::project(&unp_q.point);
    ok &= unp_q.on_curve;

    p = p.add(&q);

    f25519::Int rhs = c25519::pack(&&c25519::unproject(&p));

    return (bool)(ok & f25519::eq(&lhs, &rhs));
}

// Base point for Ed25519. Generate a subgroup of order 2^252+0x14def9dea2f79cd65812631a5cf5d3ed
const Projection BASE @private = {
    {
        0x1a, 0xd5, 0x25, 0x8f, 0x60, 0x2d, 0x56, 0xc9,
        0xb2, 0xa7, 0x25, 0x95, 0x60, 0xc7, 0x2c, 0x69,
        0x5c, 0xdc, 0xd6, 0xfd, 0x31, 0xe2, 0xa4, 0xc0,
        0xfe, 0x53, 0x6e, 0xcd, 0xd3, 0x36, 0x69, 0x21
    },
    {
        0x58, 0x66, 0x66, 0x66, 0x66, 0x66, 0x66, 0x66,
        0x66, 0x66, 0x66, 0x66, 0x66, 0x66, 0x66, 0x66,
        0x66, 0x66, 0x66, 0x66, 0x66, 0x66, 0x66, 0x66,
        0x66, 0x66, 0x66, 0x66, 0x66, 0x66, 0x66, 0x66
    },
    {
        0xa3, 0xdd, 0xb7, 0xa5, 0xb3, 0x8a, 0xde, 0x6d,
        0xf5, 0x52, 0x51, 0x77, 0x80, 0x9f, 0xf0, 0x20,
        0x7d, 0xe3, 0xab, 0x64, 0x8e, 0x4e, 0xea, 0x66,
        0x65, 0x76, 0x8b, 0xd7, 0x0f, 0x5f, 0x87, 0x67
    },
    f25519::ONE
};

<*
 Compute the pruned SHA-512 hash of a private key.

 @param [in] private_key
 @require private_key.len == PrivateKey.len
*>
fn char[sha512::HASH_SIZE] expand_private_key(char[] private_key) @local {
    char[*] r = sha512::hash(private_key);

    r[0] &= 0b11111000;
    r[fbase::Int.len - 1] &= 0b01111111;
    r[fbase::Int.len - 1] |= 0b01000000;

    return r;
}
