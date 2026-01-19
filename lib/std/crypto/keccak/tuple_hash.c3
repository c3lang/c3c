// Copyright (c) 2025-2026 Zack Puhl <github@xmit.xyz>. All rights reserved.
// Use of this source code is governed by the MIT license
// a copy of which can be found in the LICENSE_STDLIB file.
//
// See: NIST SP 800-185: https://nvlpubs.nist.gov/nistpubs/SpecialPublications/NIST.SP.800-185.pdf
//
module std::crypto::tuple_hash;

import std::crypto @public;

<*
 Generate and return a non-XOF TupleHash result of the specified `$outlen_bytes`.

 @param $security_level : "The security level of the XOF to use when hashing."
 @param $outlen_bytes : "The length of the XOF hash to return,"
 @param [in] data : "The data to hash."
 @param [in] extras : "Extra, ordered byte sequences to digest for the hash."
 @param optional_customization : "An optional, variable-length customization string used for further domain separation."

 @require @in($security_level, ...cshake::USABLE_LEVELS) : "Shake structures must use a valid security level."
 @require $outlen_bytes > 0 : "You must use an output length over zero bytes."

 @return "An array of `$outlen_bytes` bytes containing the hash."
*>
macro hash($security_level, $outlen_bytes, char[] data, char[]... extras, String optional_customization = "")
	=> xof($security_level, $outlen_bytes, data, ...extras, optional_customization: optional_customization, $is_xof: false);

<*
 Generate a non-XOF TupleHash result of `into.len` bytes and store the result in `into`.

 @param $security_level : "The security level of the XOF to use when hashing."
 @param [out] into : "The storage destination for the hash result. Its length determines the amount of hash bytes yielded."
 @param [in] data : "The data to hash."
 @param [in] extras : "Extra, ordered byte sequences to digest for the hash."
 @param optional_customization : "An optional, variable-length customization string used for further domain separation."

 @require @in($security_level, ...cshake::USABLE_LEVELS) : "Shake structures must use a valid security level."
 @require into.len > 0 : "You must use an output length over zero bytes."
*>
macro hash_into($security_level, char[] into, char[] data, char[]... extras, String optional_customization = "")
	=> xof_into($security_level, into, data, ...extras, optional_customization: optional_customization, $is_xof: false);

<*
 Generate and return an XOF TupleHash result of the specified `$outlen_bytes`.

 @param $security_level : "The security level of the XOF to use when hashing."
 @param $outlen_bytes : "The length of the XOF hash to return,"
 @param [in] data : "The data to hash."
 @param [in] extras : "Extra, ordered byte sequences to digest for the hash."
 @param optional_customization : "An optional, variable-length customization string used for further domain separation."
 @param $is_xof : "Set to `true` when the hash context should produce the XOF-specific result from the computation."

 @require @in($security_level, ...cshake::USABLE_LEVELS) : "Shake structures must use a valid security level."
 @require $outlen_bytes > 0 : "You must use an output length over zero bytes."

 @return "An array of `$outlen_bytes` bytes containing the hash."
*>
macro char[*] xof($security_level, $outlen_bytes, char[] data, char[]... extras, String optional_customization = "", bool $is_xof = true)
{
	char [$outlen_bytes] result;
	xof_into($security_level, result[..], data, ...extras, optional_customization: optional_customization, $is_xof: $is_xof);
	return result;
}

<*
 Generate a non-XOF TupleHash result of `into.len` bytes and store the result in `into`.

 @param $security_level : "The security level of the XOF to use when hashing."
 @param [out] into : "The storage destination for the hash result. Its length determines the amount of hash bytes yielded."
 @param [in] data : "The data to hash."
 @param [in] extras : "Extra, ordered byte sequences to digest for the hash."
 @param optional_customization : "An optional, variable-length customization string used for further domain separation."
 @param $is_xof : "Set to `true` when the hash context should produce the XOF-specific result from the computation."

 @require @in($security_level, ...cshake::USABLE_LEVELS) : "Shake structures must use a valid security level."
 @require into.len > 0 : "You must use an output length over zero bytes."
*>
macro void xof_into($security_level, char[] into, char[] data, char[]... extras, String optional_customization = "", bool $is_xof = true)
{
	TupleHash{$security_level} c;
	c.init(optional_customization, $is_xof);
	c.update(data, ...extras);
	c.final(into);
}

<*
 A struct wrapping the cSHAKE construct. Maintained as its own full structure for member flexibility.

 The XOF variant of TupleHash uses a `right_encode(0)` at the end of the input stream. This causes the
 output value to be completely different and not dependent on the length of the output buffer. To better
 understand this, or to see the reference, search for NIST SP 800-185.

 @require @in(SECURITY_LEVEL, ...cshake::USABLE_LEVELS) : "Shake structures must use a valid security level."
*>
struct TupleHashXOF <SECURITY_LEVEL>
{
	inline TupleHash{SECURITY_LEVEL} t;
}

<*
 Initialize the TupleHashXOF context. This is simply a wrapper around the TupleHash `init` method.

 @param optional_customization : "An optional, variable-length customization string used for further domain separation."
*>
fn void TupleHashXOF.init(&self, String optional_customization = "")
{
	keccak::@init_runtime_size(self, $sizeof(*self));
	self.t.init(optional_customization, true);
}

<*
 A struct wrapping the cSHAKE construct. Maintained as its own full structure for member flexibility.

 @require @in(SECURITY_LEVEL, ...cshake::USABLE_LEVELS) : "Shake structures must use a valid security level."
*>
struct TupleHash <SECURITY_LEVEL>
{
	inline CShake{SECURITY_LEVEL} c;
	bool is_xof_mode;
}

<*
 Initialize the TupleHash context.

 @param optional_customization : "An optional, variable-length customization string used for further domain separation."
 @param is_xof_mode : "Whether the output from the function should be the XOF variant."
*>
fn void TupleHash.init(&self, String optional_customization = "", bool is_xof_mode = false)
{
	keccak::@init_runtime_size(self, $sizeof(*self));
	self.c.init(TUPLE_HASH, optional_customization);
	self.is_xof_mode = is_xof_mode;
}

<*
 This is the more distinct feature of TupleHash: inputs are domain-separated So, calling `th.update("abc")`
 and then `th.update("def")` produces a ***different hash*** than calling `th.update("ab")` then `th.update("cdef")`.

 @param [in] data : "The data to consume with the hash context."
 @param [in] extra : "Any extra byte sequences to consume, in the precise order they should be consumed."
*>
fn void TupleHash.update(&self, char[] data, char[]... extra)
{
	self.c.update(nist::encode_length(LEFT, data.len));
	self.c.update(data);

	foreach (v : extra)
	{
		self.c.update(nist::encode_length(LEFT, v.len));
		self.c.update(v);
	}
}

<*
 An override of the Keccak `final` method for TupleHash.

 @require out.len > 0 : "You must use an output length over zero bytes."
*>
fn void TupleHash.final(&self, char[] out)
{
	self.c.update(self.is_xof_mode ? nist::ZERO_RIGHT : nist::encode_length(RIGHT, out.len));
	self.c.final(out);
}
