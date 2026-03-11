// Copyright (c) 2026 Zack Puhl <github@xmit.xyz>. All rights reserved.
// Use of this source code is governed by the MIT license
// a copy of which can be found in the LICENSE_STDLIB file.
//
// A very fast, painfully simple PRNG that should only be used for less demanding PRNG tasks
//   like deriving seeds for more complex PRNGs.
// See: https://rosettacode.org/wiki/Pseudo-random_numbers/Splitmix64
//
module std::math::random;

typedef SplitMix64 (Random) @constinit = ulong;

const ulong GOLDEN_RATIO_64 = 0x9e3779b97f4a7c15UL;
const ulong SILVER_RATIO_64 = 0x6a09e667f3bcc909UL;

fn void SplitMix64.set_seed(&self, char[] seed) @dynamic
{
	*self = (SplitMix64)(random::make_seed(ulong, seed));
}

<* Directly set the underlying `ulong` so as to manually initialize or seed the PRNG. *>
fn void SplitMix64.set(&self, ulong state) => *self = (SplitMix64)state;

fn ulong SplitMix64.next_long(&self) @dynamic
{
	ulong z = (*(ulong*)self += GOLDEN_RATIO_64);
	return mix_stafford13(z);
}

<*
 Magic numbers sourced from our friends at openjdk - where a lot of this Xorshift stuff lives.
    See: https://github.com/openjdk/jdk/blob/814b7f5d2a76288b6a8d6510a10c0241fdb727d0/src/java.base/share/classes/jdk/internal/util/random/RandomSupport.java#L811
*>
fn ulong mix_stafford13(ulong z)
{
	z = (z ^ (z >> 30)) * 0xbf58476d1ce4e5b9UL;
	z = (z ^ (z >> 27)) * 0x94d049bb133111ebUL;
	return z ^ (z >> 31);
}

<*
 @require bytes.len > 0
*>
fn void SplitMix64.next_bytes(&self, char[] bytes) @dynamic => @random_value_to_bytes(self.next_long, bytes);
fn uint128 SplitMix64.next_int128(&self) @dynamic => @long_to_int128(self.next_long());
fn uint SplitMix64.next_int(&self) @dynamic => (uint)self.next_long();
fn ushort SplitMix64.next_short(&self) @dynamic => (ushort)self.next_long();
fn char SplitMix64.next_byte(&self) @dynamic => (char)self.next_long();
