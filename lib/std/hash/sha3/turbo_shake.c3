// Copyright (c) 2025-2026 Zack Puhl <github@xmit.xyz>. All rights reserved.
// Use of this source code is governed by the MIT license
// a copy of which can be found in the LICENSE_STDLIB file.
//
// For KT and TurboSHAKE hashes, see: https://datatracker.ietf.org/doc/draft-irtf-cfrg-kangarootwelve/
//
//   TurboSHAKE allows the user to specify their own DELIMITER value at runtime.
//
//   This should remain a separate module, even thoguh it's almost the same as SHAKE, lest this
//   become very confusing to maintainers. It's not worth folding this into SHAKE to save a few
//   extra lines.
//
module std::hash::sha3::turbo_shake;

import std::hash::sha3::shake;

const DEFAULT_DELIMITER = 0x1f;
const DEFAULT_ROUNDS = 12;

alias hash = xof;
alias hash_into = xof_into;

<*
 @param $security_level : "The security level of the XOF to use when hashing."
 @param $outlen_bytes : "The length of the XOF hash to return,"
 @param [in] data : "The data to hash."
 @param delimiter_override : "An optional runtime override of the delimiter used in hash computation."

 @require @in($security_level, ...shake::USABLE_LEVELS) : "Shake structures must use a valid security level."
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

 @require @in($security_level, ...shake::USABLE_LEVELS) : "Shake structures must use a valid security level."
 @require into.len > 0 : "The destination slice must have a length greater than zero."
*>
macro void xof_into($security_level, char[] into, char[] data, char delimiter_override = DEFAULT_DELIMITER)
{
	TurboShake{$security_level} s @noinit;
	defer s.wipe();
	s.init(delimiter_override);
	s.update(data);
	s.final(into[..]);
}

struct TurboShake @generic(SECURITY_LEVEL)
{
	inline Shake{SECURITY_LEVEL} s;
}

fn void TurboShake.init(&self, char delimiter_override = DEFAULT_DELIMITER) @inline
{
	*self = {};
	self.s.init(DEFAULT_ROUNDS, delimiter_override);
}
