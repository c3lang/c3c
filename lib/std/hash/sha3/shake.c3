// Copyright (c) 2025-2026 Zack Puhl <github@xmit.xyz>. All rights reserved.
// Use of this source code is governed by the MIT license
// a copy of which can be found in the LICENSE_STDLIB file.
//
module std::hash::sha3::shake;

import std::hash::sha3::keccak;

const usz[] USABLE_LEVELS = { 128, 256 };

const DEFAULT_DELIMITER = 0x1f;
const DEFAULT_ROUNDS = keccak::DEFAULT_ROUNDS;

alias hash = xof;
alias hash_into = xof_into;

<*
 @param $security_level : "The security level of the XOF to use when hashing."
 @param $outlen_bytes : "The length of the XOF hash to return,"
 @param [in] data : "The data to hash."
 @param delimiter_override : "An optional runtime override of the delimiter used in hash computation."

 @require @in($security_level, ...USABLE_LEVELS) : "Shake structures must use a valid security level."
 @require $outlen_bytes > 0 : "You must use an output length over zero bytes."
*>
macro char[*] xof($security_level, $outlen_bytes, char[] data, char delimiter_override = DEFAULT_DELIMITER)
{
	char[$outlen_bytes] into;
	xof_into($security_level, into[..], data, delimiter_override);
	return into;
}

<*
 @param $security_level : "The security level of the XOF to use when hashing."
 @param [out] into : "The destination for the generated variable-length hash."
 @param [in] data : "The data to hash."
 @param delimiter_override : "An optional runtime override of the delimiter used in hash computation."

 @require @in($security_level, ...USABLE_LEVELS) : "Shake structures must use a valid security level."
 @require into.len > 0 : "The destination slice must have a length greater than zero."
*>
macro void xof_into($security_level, char[] into, char[] data, char delimiter_override = DEFAULT_DELIMITER)
{
	Shake{$security_level} s @noinit;
	defer s.wipe();
	s.init(DEFAULT_ROUNDS, delimiter_override);
	s.update(data);
	s.final(into[..]);
}

<*
 A structure with a variable amount of rounds and a variable compile-time delimiter value.

 @require @in(SECURITY_LEVEL, ...USABLE_LEVELS) : "Shake structures must use a valid security level."
*>
struct Shake @generic(SECURITY_LEVEL)
{
	inline Keccak{SECURITY_LEVEL} k;
	bool is_padded;
	usz offset;
	char[keccak::rate(SECURITY_LEVEL)] buf;
}

fn void Shake.init(&self, char rounds_override = DEFAULT_ROUNDS, char delimiter_override = DEFAULT_DELIMITER) @inline
{
	*self = {};
	self.k.init(rounds_override, delimiter_override);
}

fn void Shake.pad(&self) => unreachable("Shake.pad() must never be accessed directly");

<*
 @param [out] into : "The destination for the generated variable-length hash."
*>
fn void Shake.squeeze(&self, char[] into)
{
	if (!self.is_padded)
	{
		self.k.pad();
		self.is_padded = true;
	}

	char[] out = into;
	if (self.offset > 0)
	{
		usz left = min(self.buf.len - self.offset, out.len);
		if (left > 0)
		{
			out[:left] = self.buf[self.offset:left];
			out = out[left..];
			self.offset += left;
			if (out.len == 0) return;
		}
	}

	char[] full_blocks = out[:(out.len - out.len % keccak::rate(SECURITY_LEVEL))];
	if (full_blocks.len > 0)
	{
		self.k.squeeze(full_blocks[..]);
		out = out[full_blocks.len..];
	}
	if (out.len > 0)
	{
		self.k.squeeze(self.buf[..]);
		out[..] = self.buf[:out.len];
		self.offset = out.len;
	}
}
