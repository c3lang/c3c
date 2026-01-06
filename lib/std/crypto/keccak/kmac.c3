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
*>
macro hash($security_level, $outlen_bytes, char[] key, char[] data, String optional_customization = "")
	=> xof($security_level, $outlen_bytes, key, data, optional_customization, false);

macro hash_into($security_level, char[] into, char[] key, char[] data, String optional_customization = "")
	=> xof_into($security_level, into, key, data, optional_customization, false);

macro char[*] xof($security_level, $outlen_bytes, char[] key, char[] data, String optional_customization = "", bool $is_xof = true)
{
	char [$outlen_bytes] result @noinit;
	xof_into($security_level, result[..], key, data, optional_customization, $is_xof);
	return result;
}

macro void xof_into($security_level, char[] into, char[] key, char[] data, String optional_customization = "", bool $is_xof = true)
{
	Kmac{$security_level} c;
	c.init(key, optional_customization, $is_xof);
	c.update(data);
	c.final(into);
}

<*
 A struct wrapping the vanilla KMAC construct. Maintained as its own full structure for member flexibility.

 @require @in(SECURITY_LEVEL, ...cshake::USABLE_LEVELS) : "Shake structures must use a valid security level."
*>
struct KmacXOF @generic(SECURITY_LEVEL)
{
	inline Kmac{SECURITY_LEVEL} k;
}

<*
 A wrapper initialization for the KMAC XOF construct which transparently calls the Kmac initializer.

 @param [in] key : "The key used to seed the KMAC cipher."
 @param optional_customization : "An optional, variable-length customization string used for further domain separation."
*>
fn void KmacXOF.init(&self, char[] key, String optional_customization = "") @inline
{
	keccak::@init_runtime_size(self, $sizeof(*self));
	self.k.init(key, optional_customization, is_xof_mode: true);
}

<*
 A struct wrapping the cSHAKE construct. Maintained as its own full structure for member flexibility.

 @require @in(SECURITY_LEVEL, ...cshake::USABLE_LEVELS) : "Shake structures must use a valid security level."
*>
struct Kmac @generic(SECURITY_LEVEL)
{
	inline CShake{SECURITY_LEVEL} c;
	bool is_xof_mode;
}

<*
 Initialize the vanilla KMAC context.

 @param [in] key : "The key used to seed the KMAC cipher."
 @param optional_customization : "An optional, variable-length customization string used for further domain separation."
 @param is_xof_mode : "Controls whether the hash context will output an XOF-based hash value or a regular hash value."
*>
fn void Kmac.init(&self, char[] key, String optional_customization = "", bool is_xof_mode = false)
{
	keccak::@init_runtime_size(self, $sizeof(*self));
	self.c.init(KMAC, optional_customization);
	self.is_xof_mode = is_xof_mode;

	self.c.update(nist::encoded_rate_bytes(SECURITY_LEVEL));

	self.c.update(nist::encode_length(LEFT, key.len));
	self.c.update(key);

	self.c.fill_block();
}

<*
 @require out.len > 0
*>
fn void Kmac.final(&self, char[] out)
{
	defer self.finalize();
	self.squeeze(out);
}

<*
 @require out.len > 0
*>
fn void Kmac.squeeze(&self, char[] out)
{
	if (!self.c.is_padded)   // becomes 'true' after the first 'squeeze' call below
	{
		if (self.is_xof_mode)
		{
			self.c.update(nist::ZERO_RIGHT);
		}
		else
		{
			self.c.update(nist::encode_length(RIGHT, out.len));
		}
	}

	self.c.squeeze(out);
}
