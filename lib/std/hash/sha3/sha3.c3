// Copyright (c) 2025-2026 Zack Puhl <github@xmit.xyz>. All rights reserved.
// Use of this source code is governed by the MIT license
// a copy of which can be found in the LICENSE_STDLIB file.
//
module std::hash::sha3::sha3;

import std::hash::sha3::keccak;

const usz[] DIGEST_SIZES = keccak::SECURITY_LEVELS;
const DEFAULT_DELIMITER = 0x06;

<*
@require @in(HASH_BITS, ...DIGEST_SIZES) : "You must use a valid SHA-3 digest size."
*>
fn char[HASH_BITS / 8] hash(char[] data) @generic(HASH_BITS)
{
	char[HASH_BITS / 8] result;
	Sha3{HASH_BITS} s @noinit;
	s.init();
	s.update(data);
	s.final(result[..]);
	return result;
}

<*
@require @in(HASH_BITS, ...DIGEST_SIZES) : "You must use a valid SHA-3 digest size."
*>
struct Sha3 @generic(HASH_BITS)
{
	inline Keccak{HASH_BITS} k;
}

fn void Sha3.init(&self) @inline
{
	mem::zero_volatile(@as_char_view(*self));
	self.k.init(runtime_delimiter: DEFAULT_DELIMITER);
}
