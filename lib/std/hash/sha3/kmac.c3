// Copyright (c) 2025-2026 Zack Puhl <github@xmit.xyz>. All rights reserved.
// Use of this source code is governed by the MIT license
// a copy of which can be found in the LICENSE_STDLIB file.
//
// See: NIST SP 800-185: https://nvlpubs.nist.gov/nistpubs/SpecialPublications/NIST.SP.800-185.pdf
//
module std::hash::sha3::kmac;

import std::hash::sha3::keccak;
import std::hash::sha3::cshake;
import std::hash::sha3::nist @public;

macro hash($security_level, $outlen_bytes, char[] key, char[] data, String optional_customization = "")
	=> xof($security_level, $outlen_bytes, key, data, optional_customization, false);

macro hash_into($security_level, char[] into, char[] key, char[] data, String optional_customization = "")
	=> xof_into($security_level, into, key, data, optional_customization, false);

macro char[*] xof($security_level, $outlen_bytes, char[] key, char[] data, String optional_customization = "", bool $is_xof = true)
{
	char [$outlen_bytes] result;
	xof_into($security_level, result[..], key, data, optional_customization, $is_xof);
	return result;
}

macro void xof_into($security_level, char[] into, char[] key, char[] data, String optional_customization = "", bool $is_xof = true)
{
	Kmac{$security_level} c @noinit;
	defer c.wipe();
	c.init(key, optional_customization, $is_xof);
	c.update(data);
	c.final(into);
}

struct KmacXOF @generic(SECURITY_LEVEL)
{
	inline Kmac{SECURITY_LEVEL} k;
}

fn void KmacXOF.init(&self, char[] key, String optional_customization = "") @inline
	=> self.k.init(key, optional_customization, true);

struct Kmac @generic(SECURITY_LEVEL)
{
	inline CShake{SECURITY_LEVEL} c;
	bool is_xof_mode;
}

fn void Kmac.init(&self, char[] key, String optional_customization = "", bool is_xof_mode = false)
{
	self.is_xof_mode = is_xof_mode;
	self.c.init(KMAC, optional_customization);

	self.c.update(nist::@ct_encode_length(NistEncodingType.LEFT, keccak::rate(SECURITY_LEVEL) / 8));

	self.c.update(nist::encode_length(NistEncodingType.LEFT, key.len));
	self.c.update(key);

	self.c.fill_block();
}

<*
 @require out.len > 0
*>
macro void Kmac.final(&self, char[] out)
{
	defer self.wipe();
	self.squeeze(out);
}

<*
 @require out.len > 0
*>
macro void Kmac.squeeze(&self, char[] out)
{
	if (!self.c.is_padded)   // becomes 'true' after the first 'squeeze' call below
	{
		if (self.is_xof_mode)
		{
			self.c.update(nist::@ct_encode_length(NistEncodingType.RIGHT, 0));
		}
		else
		{
			self.c.update(nist::encode_length(NistEncodingType.RIGHT, out.len));
		}
	}

	self.c.squeeze(out);
}
