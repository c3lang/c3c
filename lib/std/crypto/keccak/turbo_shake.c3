// Copyright (c) 2025-2026 Zack Puhl <github@xmit.xyz>. All rights reserved.
// Use of this source code is governed by the MIT license
// a copy of which can be found in the LICENSE_STDLIB file.
//
// For KT and TurboSHAKE hashes, see: https://www.rfc-editor.org/rfc/rfc9861.html
//
//   TurboSHAKE allows the user to specify their own DELIMITER value at runtime.
//
//   This should remain a separate module, even thoguh it's almost the same as SHAKE, lest this
//   become very confusing to maintainers. It's not worth folding this into SHAKE to save a few
//   extra lines.
//
module std::crypto::turbo_shake;

import std::crypto @public;

<* The Keccak domain-separation delimiter to use specifically for TurboSHAKE. *>
const DEFAULT_DELIMITER = 0x1f;
<* The amount of Keccak rounds to use specifically for TurboSHAKE. *>
const DEFAULT_ROUNDS = 12;

<*
 Generate and return an XOF result of the specified `$outlen_bytes`.

 @param $security_level : "The security level of the XOF to use when hashing."
 @param $outlen_bytes : "The length of the XOF hash to return,"
 @param [in] data : "The data to hash."
 @param delimiter_override : "An optional runtime override of the delimiter used in hash computation."

 @require @in($security_level, ...shake::USABLE_LEVELS) : "Shake structures must use a valid security level."
 @require $outlen_bytes > 0 : "You must use an output length over zero bytes."

 @return "An array of `$outlen_bytes` bytes containing the hash."
*>
macro char[*] xof($security_level, $outlen_bytes, char[] data, char delimiter_override = DEFAULT_DELIMITER)
{
	char[$outlen_bytes] into @noinit;
	xof_into($security_level, into[..], data, delimiter_override);
	return into;
}

<* When hashing with TurboSHAKE, it's the same thing as using the XOF construct. *>
alias hash = xof;

<*
 Generate an XOF result of `into.len` bytes and store the result in the `into` slice.

 @param $security_level : "The security level of the XOF to use when hashing."
 @param [out] into : "The destination for the generated variable-length hash."
 @param [in] data : "The data to hash."
 @param delimiter_override : "An optional runtime override of the delimiter used in hash computation."

 @require @in($security_level, ...shake::USABLE_LEVELS) : "Shake structures must use a valid security level."
 @require into.len > 0 : "The destination slice must have a length greater than zero."
*>
macro void xof_into($security_level, char[] into, char[] data, char delimiter_override = DEFAULT_DELIMITER)
{
	TurboShake{$security_level} s;
	s.init(delimiter_override);
	s.update(data);
	s.final(into);
}

<* When hashing with TurboSHAKE, it's the same thing as using the XOF construct. *>
alias hash_into = xof_into;

<*
 A struct wrapping the SHAKE construct. Maintained as its own full structure for member flexibility.

 @require @in(SECURITY_LEVEL, ...shake::USABLE_LEVELS) : "Shake structures must use a valid security level."
*>
struct TurboShake <SECURITY_LEVEL>
{
	inline Shake{SECURITY_LEVEL} s;
}

<*
 Initialize the TurboSHAKE context.

 @param delimiter_override : "An optional runtime override of the delimiter used in hash computation."
*>
fn void TurboShake.init(&self, char delimiter_override = DEFAULT_DELIMITER)
{
	keccak::@init_runtime_size(self, $sizeof(*self));
	self.s.init(DEFAULT_ROUNDS, delimiter_override);
}
