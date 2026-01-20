// Copyright (c) 2025-2026 Zack Puhl <github@xmit.xyz>. All rights reserved.
// Use of this source code is governed by the MIT license
// a copy of which can be found in the LICENSE_STDLIB file.
//
// See: NIST SP 800-185: https://nvlpubs.nist.gov/nistpubs/SpecialPublications/NIST.SP.800-185.pdf
//
module std::crypto::kmac;

import std::crypto @public;

<*
 Generate and return a non-XOF KMAC hash of the specified `$outlen_bytes` length.

 @param $security_level : "The security level of the XOF to use when hashing."
 @param $outlen_bytes : "The length of the XOF hash to return,"
 @param [in] key : "The key used to authenticate the data."
 @param [in] data : "The data to hash."
 @param customizer : "An optional, variable-length customization string used for further domain separation."

 @require @in($security_level, ...cshake::USABLE_LEVELS) : "Shake and Shake-derived structures must use a valid security level."
 @require $outlen_bytes > 0 : "You must use an output length over zero bytes."

 @return "An array of `$outlen_bytes` bytes containing the hash."
*>
macro hash($security_level, $outlen_bytes, char[] key, char[] data, String customizer = "")
	=> xof($security_level, $outlen_bytes, key, data, customizer: customizer, $is_xof: false);

<*
 Generate a non-XOF KMAC hash of `into.len` bytes and store the result in the `into` slice..

 @param $security_level : "The security level of the XOF to use when hashing."
 @param [out] into : "The storage destination for the KMAC result. Its length determines the amount of KMAC bytes yielded."
 @param [in] key : "The key used to authenticate the data."
 @param [in] data : "The data to hash."
 @param customizer : "An optional, variable-length customization string used for further domain separation."

 @require @in($security_level, ...cshake::USABLE_LEVELS) : "Shake and Shake-derived structures must use a valid security level."
 @require into.len > 0 : "You must use an output length over zero bytes."
*>
macro hash_into($security_level, char[] into, char[] key, char[] data, String customizer = "")
	=> xof_into($security_level, into, key, data, customizer: customizer, $is_xof: false);

<*
 Generate and return an XOF KMAC hash of the specified `$outlen_bytes` length.

 @param $security_level : "The security level of the XOF to use when hashing."
 @param $outlen_bytes : "The length of the XOF hash to return,"
 @param [in] key : "The key used to authenticate the data."
 @param [in] data : "The data to hash."
 @param customizer : "An optional, variable-length customization string used for further domain separation."
 @param $is_xof : "Used by the `hash` and `hash_into` macros to wrap what this function already does."

 @require @in($security_level, ...cshake::USABLE_LEVELS) : "Shake and Shake-derived structures must use a valid security level."
 @require $outlen_bytes > 0 : "You must use an output length over zero bytes."

 @return "An array of `$outlen_bytes` bytes containing the hash."
*>
macro char[*] xof($security_level, $outlen_bytes, char[] key, char[] data, String customizer = "", bool $is_xof = true)
{
	char [$outlen_bytes] result @noinit;
	xof_into($security_level, result[..], key, data, customizer: customizer, $is_xof: $is_xof);
	return result;
}

<*
 Generate an XOF KMAC hash of `into.len` bytes and store the result in the `into` slice.

 @param $security_level : "The security level of the XOF to use when hashing."
 @param [out] into : "The storage destination for the KMAC result. Its length determines the amount of KMAC bytes yielded."
 @param [in] key : "The key used to authenticate the data."
 @param [in] data : "The data to hash."
 @param customizer : "An optional, variable-length customization string used for further domain separation."
 @param $is_xof : "Used by the `hash` and `hash_into` macros to wrap what this function already does."

 @require @in($security_level, ...cshake::USABLE_LEVELS) : "Shake and Shake-derived structures must use a valid security level."
 @require into.len > 0 : "You must use an output length over zero bytes."
*>
macro void xof_into($security_level, char[] into, char[] key, char[] data, String customizer = "", bool $is_xof = true)
{
	Kmac{$security_level} c;
	c.init(key, customizer: customizer, is_xof_mode: $is_xof);
	c.update(data);
	c.final(into);
}

<*
 A struct wrapping the vanilla KMAC construct. Maintained as its own full structure for member flexibility.

 @require @in(SECURITY_LEVEL, ...cshake::USABLE_LEVELS) : "Shake and Shake-derived structures must use a valid security level."
*>
struct KmacXOF <SECURITY_LEVEL>
{
	inline Kmac{SECURITY_LEVEL} k;
}

<*
 A wrapper initialization for the KMAC XOF construct which transparently calls the Kmac initializer.

 @param [in] key : "The key used to seed the KMAC cipher."
 @param customizer : "An optional, variable-length customization string used for further domain separation."
*>
fn void KmacXOF.init(&self, char[] key, String customizer = "")
{
	keccak::@init_runtime_size(self, $sizeof(*self));
	self.k.init(key, customizer: customizer, is_xof_mode: true);
}

<*
 A struct wrapping the cSHAKE construct. Maintained as its own full structure for member flexibility.

 @require @in(SECURITY_LEVEL, ...cshake::USABLE_LEVELS) : "Shake and Shake-derived structures must use a valid security level."
*>
struct Kmac <SECURITY_LEVEL>
{
	inline CShake{SECURITY_LEVEL} c;
	bool is_xof_mode;
}

<*
 Initialize the vanilla KMAC context.

 @param [in] key : "The key used to seed the KMAC cipher."
 @param customizer : "An optional, variable-length customization string used for further domain separation."
 @param is_xof_mode : "Controls whether the hash context will output an XOF-based hash value or a regular hash value."
*>
fn void Kmac.init(&self, char[] key, String customizer = "", bool is_xof_mode = false)
{
	keccak::@init_runtime_size(self, $sizeof(*self));
	self.c.init(KMAC, customizer);
	self.is_xof_mode = is_xof_mode;

	self.c.update(nist::encoded_rate_bytes(SECURITY_LEVEL));

	self.c.update(nist::encode_length(LEFT, key.len));
	self.c.update(key);

	self.c.fill_block();
}

<*
 Override the Keccak finalization process by just squeezing the "sponge" and crystallizing.

 @param [out] out : "The destination slice for the KMAC result. The slice length determines the amount of XOF bytes yielded."

 @require out.len > 0 : "You must use an output length over zero bytes."
*>
fn void Kmac.final(&self, char[] out)
{
	defer self.finalize();
	self.squeeze(out);
}

<*
 Override the Keccak squeeze process. Does NOT finalize the KMAC context being used.

 @param [out] out : "The destination slice for the KMAC result. The slice length determines the amount of XOF bytes yielded."

 @require out.len > 0 : "You must use an output length over zero bytes."
*>
fn void Kmac.squeeze(&self, char[] out)
{
	if (!self.c.is_padded)   // becomes 'true' after the first 'squeeze' call below
	{
		self.c.update(self.is_xof_mode ? nist::ZERO_RIGHT : nist::encode_length(RIGHT, out.len));
	}
	self.c.squeeze(out);
}
