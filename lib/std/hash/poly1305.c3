// Copyright (c) 2025 Zack Puhl <github@xmit.xyz>. All rights reserved.
// Use of this source code is governed by the MIT license
// a copy of which can be found in the LICENSE_STDLIB file.
//
// Poly1305 code dedicated from repo: https://github.com/NotsoanoNimus/chacha20_aead.c3l (but massively cleaned)
module std::hash::poly1305;


<* The fixed output length of Poly1305 tags (hashes).  *>
const TAG_SIZE = 16;

<* Fixed length of a Poly1305 block. *>
const BLOCK_SIZE = 16;

<* Fixed length of the required Poly1305 key, *>
const KEY_SIZE = 32;


struct Poly1305
{
    ulong[3]        h; // hash internal state
    uint128         r; // secret portion of key
    uint128         nonce; // initialization vector, derived from key
    char[TAG_SIZE]  temp; // last partial ingestion state
    usz             num; // index into last partial ingestion
    // Additional, cached state information:
    ulong           r0;
    ulong           r1;
    ulong           s1;
}


<* Constant-time carrying computation for block permutations. *>
macro ulong constant_time_carry(ulong a, ulong b) @local
{
    return (a ^ ((a ^ b) | ((a - b) ^ b))) >> (bitsizeof(ulong) - 1);
}


<*
 Compute a Poly1305 message authentication code for the input with the given key (secret + nonce) value.
 Note that this construct SHOULD NOT be used with the 'Hmac' module; it is its own MAC package.

 @param[in] input
 @param key
*>
fn char[TAG_SIZE] hash(char[] input, char[KEY_SIZE] key)
{
    Poly1305 p @noinit;
    p.init(key);
    p.update(input);
    return p.final();
}

<* Alias for the `hash` function; Message Authentication Code. *>
alias mac = hash;

<* Alias for the `hash` function; "tag" generation. *>
alias tag = hash;


fn void Poly1305.init(&self, char[KEY_SIZE] key)
{
    *self = { // implicitly clears state as well
        .r      = mem::load((uint128*)&key[ 0], 1) & 0x0ffffffc_0ffffffc_0ffffffc_0fffffff, // clamped per spec
        .nonce  = mem::load((uint128*)&key[16], 1)
    };

    self.r0 = @unaligned_load(((ulong*)&self.r)[0], 1);
    self.r1 = @unaligned_load(((ulong*)&self.r)[1], 1);

    self.s1 = self.r1 + (self.r1 >> 2);
}


fn void Poly1305.update(&self, char[] input)
{
    if (self.num) // currently between consuming full blocks?
    {
        usz rem = BLOCK_SIZE - self.num;
        if (input.len < rem)
        {
            self.temp[self.num:input.len] = input[..]; // saving another partial block
            self.num += input.len; // move index forward
            return;
        }
        // ingest up to a block size to finish the partial, then advance the slice ptr
        self.temp[self.num:rem] = input[:rem];
        self.blocks(self.temp[..]);
        input = input[rem..];
    }

    usz even_length = input.len - (input.len % BLOCK_SIZE);
    if (even_length >= BLOCK_SIZE)
    {
        self.blocks(input[:even_length]); // consume blocks
        input = input[even_length..]; // scroll to end (remainder)
    }

    if (input.len) self.temp[:input.len] = input[..]; // keep remainder (uneven block sizes)

    self.num = input.len;
}


fn char[TAG_SIZE] Poly1305.final(&self)
{
    if (self.num) // consume any leftovers
    {
        self.temp[self.num++] = 1; // partial blocks must end with 0x01
        self.temp[self.num..] = {}; // explicit zeros on the rest
        self.blocks(self.temp[..], 0); // chomp
    }

    uint128 t = (uint128)self.h[0] + 5;
    ulong g0 = (ulong)t;

    t = (uint128)self.h[1] + (t >> 64);
    ulong g1 = (ulong)t;

    ulong mask = 0 - ((self.h[2] + (ulong)(t >> 64)) >> 2);
    self.h[0] = (self.h[0] & ~mask) | (g0 & mask);
    self.h[1] = (self.h[1] & ~mask) | (g1 & mask);

    t = (uint128)self.h[0] + (ulong)self.nonce;
    self.h[0] = (ulong)t;

    t = (uint128)self.h[1] + (ulong)(self.nonce >> 64) + (t >> 64);
    self.h[1] = (ulong)t;

    // Store, clear context, return.
    uint128 result = ((uint128)self.h[1] << 64) + self.h[0];
    *self = {};
    return @as_char_view(result)[:TAG_SIZE];
}


fn void Poly1305.blocks(&self, char[] input, ulong pad_bit = 1) @local
{
    for (; input.len >= BLOCK_SIZE; input = input[BLOCK_SIZE..])
    {
        ulong i0 = mem::load((ulong*)&input[0], 1);
        ulong i1 = mem::load((ulong*)&input[8], 1);

        uint128 d0 = (uint128)self.h[0] + i0;
        self.h[0] = (ulong)d0;

        uint128 d1 = (uint128)self.h[1] + (d0 >> 64) + i1;
        self.h[1] = (ulong)d1;

        self.h[2] += (ulong)(d1 >> 64) + pad_bit;

        d0 = ((uint128)self.h[0] * self.r0) + ((uint128)self.h[1] * self.s1);
        d1 = ((uint128)self.h[0] * self.r1) + ((uint128)self.h[1] * self.r0) + ((uint128)self.h[2] * self.s1);
        self.h[2] = (self.h[2] * self.r0);

        self.h[0] = (ulong)d0;

        d1 = d1 + (d0 >> 64);
        self.h[1] = (ulong)d1;
        self.h[2] = self.h[2] + (ulong)(d1 >> 64);

        ulong c = (self.h[2] >> 2) + (self.h[2] & ~(ulong)3);

        self.h[2] &= 3;
        self.h[0] += c;
        c = constant_time_carry(self.h[0], c);
        self.h[1] += c;
        self.h[2] += constant_time_carry(self.h[1], c);
    }
}
