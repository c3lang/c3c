/**
 * SHA-512 implementation for the C3 stdlib.
 *
 * The core components are based almost exclusively on the musl libc sha512 implementation:
 *    https://git.musl-libc.org/cgit/musl/commit/src/misc/crypt_sha512.c?id=88bf5a8a8d7d796f63cca8589f4de67aa8345f1a
 *
 * SHA-384, 512/224, and 512/256 are very simple addenda to this module.
 *
 */
module std::hash::sha512;

import std::hash::hmac;


const BLOCK_SIZE = 128;
const HASH_SIZE = 64;

struct Sha512
{
    ulong               length;
    ulong[8]            hash_state;
    char[BLOCK_SIZE]    buffer;
}

alias HmacSha512    = Hmac{Sha512, HASH_SIZE, BLOCK_SIZE};
alias hmac          = hmac::hash{Sha512, HASH_SIZE, BLOCK_SIZE};
alias pbkdf2        = hmac::pbkdf2{Sha512, HASH_SIZE, BLOCK_SIZE};

macro ulong ror(ulong n, int k) @local => ((n >> k) | (n << (64 - k)));

macro ulong ch(ulong x, ulong y, ulong z) @local => (z ^ (x & (y ^ z)));
macro ulong maj(ulong x, ulong y, ulong z) @local => ((x & y) | (z & (x | y)));
macro ulong s0(ulong x) @local => (ror(x, 28) ^ ror(x, 34) ^ ror(x, 39));
macro ulong s1(ulong x) @local => (ror(x, 14) ^ ror(x, 18) ^ ror(x, 41));
macro ulong r0(ulong x) @local => (ror(x, 1) ^ ror(x, 8) ^ (x >> 7));
macro ulong r1(ulong x) @local => (ror(x, 19) ^ ror(x, 61) ^ (x >> 6));

const ulong[80] K @local = {
    0x428a2f98d728ae22, 0x7137449123ef65cd, 0xb5c0fbcfec4d3b2f, 0xe9b5dba58189dbbc,
    0x3956c25bf348b538, 0x59f111f1b605d019, 0x923f82a4af194f9b, 0xab1c5ed5da6d8118,
    0xd807aa98a3030242, 0x12835b0145706fbe, 0x243185be4ee4b28c, 0x550c7dc3d5ffb4e2,
    0x72be5d74f27b896f, 0x80deb1fe3b1696b1, 0x9bdc06a725c71235, 0xc19bf174cf692694,
    0xe49b69c19ef14ad2, 0xefbe4786384f25e3, 0x0fc19dc68b8cd5b5, 0x240ca1cc77ac9c65,
    0x2de92c6f592b0275, 0x4a7484aa6ea6e483, 0x5cb0a9dcbd41fbd4, 0x76f988da831153b5,
    0x983e5152ee66dfab, 0xa831c66d2db43210, 0xb00327c898fb213f, 0xbf597fc7beef0ee4,
    0xc6e00bf33da88fc2, 0xd5a79147930aa725, 0x06ca6351e003826f, 0x142929670a0e6e70,
    0x27b70a8546d22ffc, 0x2e1b21385c26c926, 0x4d2c6dfc5ac42aed, 0x53380d139d95b3df,
    0x650a73548baf63de, 0x766a0abb3c77b2a8, 0x81c2c92e47edaee6, 0x92722c851482353b,
    0xa2bfe8a14cf10364, 0xa81a664bbc423001, 0xc24b8b70d0f89791, 0xc76c51a30654be30,
    0xd192e819d6ef5218, 0xd69906245565a910, 0xf40e35855771202a, 0x106aa07032bbd1b8,
    0x19a4c116b8d2d0c8, 0x1e376c085141ab53, 0x2748774cdf8eeb99, 0x34b0bcb5e19b48a8,
    0x391c0cb3c5c95a63, 0x4ed8aa4ae3418acb, 0x5b9cca4f7763e373, 0x682e6ff3d6b2b8a3,
    0x748f82ee5defb2fc, 0x78a5636f43172f60, 0x84c87814a1f0ab72, 0x8cc702081a6439ec,
    0x90befffa23631e28, 0xa4506cebde82bde9, 0xbef9a3f7b2c67915, 0xc67178f2e372532b,
    0xca273eceea26619c, 0xd186b8c721c0c207, 0xeada7dd6cde0eb1e, 0xf57d4f7fee6ed178,
    0x06f067aa72176fba, 0x0a637dc5a2c898a6, 0x113f9804bef90dae, 0x1b710b35131c471b,
    0x28db77f523047d84, 0x32caab7b40c72493, 0x3c9ebe0a15c9bebc, 0x431d67c49c100d4c,
    0x4cc5d4becb3e42b6, 0x597f299cfc657e2a, 0x5fcb6fab3ad6faec, 0x6c44198c4a475817
};


// See: https://nvlpubs.nist.gov/nistpubs/FIPS/NIST.FIPS.180-4.pdf
//   All truncation types are simple to add onto the base SHA512 implementation at a (near) future time.
enum HashTruncationType : uint (uint truncation_width, ulong[8] initial_state)
{
    SHA512 = {
        512,
        {
            0x6a09e667f3bcc908,
            0xbb67ae8584caa73b,
            0x3c6ef372fe94f82b,
            0xa54ff53a5f1d36f1,
            0x510e527fade682d1,
            0x9b05688c2b3e6c1f,
            0x1f83d9abfb41bd6b,
            0x5be0cd19137e2179
        }
    },
    SHA384 = {
        384,
        {
            0xcbbb9d5dc1059ed8,
            0x629a292a367cd507,
            0x9159015a3070dd17,
            0x152fecd8f70e5939,
            0x67332667ffc00b31,
            0x8eb44a8768581511,
            0xdb0c2e0d64f98fa7,
            0x47b5481dbefa4fa4
        }
    },
    SHA512_224 = {
        224,
        {
            0x8C3D37C819544DA2,
            0x73E1996689DCD4D6,
            0x1DFAB7AE32FF9C82,
            0x679DD514582F9FCF,
            0x0F6D2B697BD44DA8,
            0x77E36F7304C48942,
            0x3F9D85A86A1D36C8,
            0x1112E6AD91D692A1
        }
    },
    SHA512_256 = {
        256,
        {
            0x22312194FC2BF72C,
            0x9F555FA3C84C64C2,
            0x2393B86B6F53B151,
            0x963877195940EABD,
            0x96283EE2A88EFFE3,
            0xBE5E1E2553863992,
            0x2B0199FC2C85B8AA,
            0x0EB72DDC81C52CA2
        }
    },
}


<*
    @param [in] data
*>
fn char[HASH_SIZE] hash(char[] data)
{
    Sha512 s @noinit;
    s.init();
    s.update(data);
    return s.final();
}


fn void Sha512.init(&self)
{
    *self = {
        .hash_state = HashTruncationType.SHA512.initial_state
    };
}


<*
    @param [in] data
    @require data.len <= ulong.max
*>
fn void Sha512.update(&self, char[] data)
{
    char* p = data.ptr;
    ulong len = data.len;
    ulong l;
    ulong r = self.length % 128;

    self.length += len;

    if (r)
    {
        if (len < (128 - r))
        {
            for (l = 0; l < len; ++l) self.buffer[r+l] = p[l];

            return;
        }

        for (l = 0; l < 128 - r; ++l) self.buffer[r+l] = p[l];

        len -= (128 - r);
        p = &p[128 - r];

        sha512_transform(&self.hash_state, &self.buffer);
    }

    for (; len >= 128; len -= 128, p = &p[128]) sha512_transform(&self.hash_state, p);

    for (l = 0; l < len; ++l) self.buffer[l] = p[l];
}


fn char[HASH_SIZE] Sha512.final(&self)
{
    char[HASH_SIZE] hash;

    int i;
    ulong r = self.length % 128;

    self.buffer[r++] = 0x80;

    if (r > 112)
    {
        for (i = 0; i < 128 - r; ++i) self.buffer[r+i] = 0;

        r = 0;

        sha512_transform(&self.hash_state, &self.buffer);
    }

    for (i = 0; i < 120 - r; ++i) self.buffer[r+i] = 0;

    self.length *= 8;

    self.buffer[120] = (char)(self.length >> 56);
    self.buffer[121] = (char)(self.length >> 48);
    self.buffer[122] = (char)(self.length >> 40);
    self.buffer[123] = (char)(self.length >> 32);
    self.buffer[124] = (char)(self.length >> 24);
    self.buffer[125] = (char)(self.length >> 16);
    self.buffer[126] = (char)(self.length >> 8);
    self.buffer[127] = (char)(self.length);

    sha512_transform(&self.hash_state, &self.buffer);

    for (i = 0; i < 8; ++i)
    {
        hash[(8 * i)]     = (char)(self.hash_state[i] >> 56);
        hash[(8 * i) + 1] = (char)(self.hash_state[i] >> 48);
        hash[(8 * i) + 2] = (char)(self.hash_state[i] >> 40);
        hash[(8 * i) + 3] = (char)(self.hash_state[i] >> 32);
        hash[(8 * i) + 4] = (char)(self.hash_state[i] >> 24);
        hash[(8 * i) + 5] = (char)(self.hash_state[i] >> 16);
        hash[(8 * i) + 6] = (char)(self.hash_state[i] >> 8);
        hash[(8 * i) + 7] = (char)(self.hash_state[i]);
    }

    return hash;
}


<*
    @param [&inout] state
    @param [&in] buf
*>
fn void sha512_transform(ulong *state, char *buf) @local
{
    ulong t1, t2, a, b, c, d, e, f, g, h;
    ulong[80] w;
    int i;

    for (i = 0; i < 16; ++i)
    {
        w[i]  = (ulong)buf[(8 * i)]     << 56;
        w[i] |= (ulong)buf[(8 * i) + 1] << 48;
        w[i] |= (ulong)buf[(8 * i) + 2] << 40;
        w[i] |= (ulong)buf[(8 * i) + 3] << 32;
        w[i] |= (ulong)buf[(8 * i) + 4] << 24;
        w[i] |= (ulong)buf[(8 * i) + 5] << 16;
        w[i] |= (ulong)buf[(8 * i) + 6] << 8;
        w[i] |= buf[(8 * i) + 7];
    }

    for (; i < 80; ++i) w[i] = r1(w[i - 2]) + w[i - 7] + r0(w[i - 15]) + w[i - 16];

    a = state[0];
    b = state[1];
    c = state[2];
    d = state[3];
    e = state[4];
    f = state[5];
    g = state[6];
    h = state[7];

    for (i = 0; i < 80; ++i)
    {
        t1 = h + s1(e) + ch(e, f, g) + K[i] + w[i];
        t2 = s0(a) + maj(a, b, c);
        h = g;
        g = f;
        f = e;
        e = d + t1;
        d = c;
        c = b;
        b = a;
        a = t1 + t2;
    }

    state[0] += a;
    state[1] += b;
    state[2] += c;
    state[3] += d;
    state[4] += e;
    state[5] += f;
    state[6] += g;
    state[7] += h;
}
