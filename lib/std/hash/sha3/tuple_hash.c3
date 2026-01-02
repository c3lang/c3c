// Copyright (c) 2025-2026 Zack Puhl <github@xmit.xyz>. All rights reserved.
// Use of this source code is governed by the MIT license
// a copy of which can be found in the LICENSE_STDLIB file.
//
// See: NIST SP 800-185: https://nvlpubs.nist.gov/nistpubs/SpecialPublications/NIST.SP.800-185.pdf
//
module std::hash::sha3::tuple_hash;

import std::hash::sha3::keccak;
import std::hash::sha3::cshake;
import std::hash::sha3::nist @public;

macro hash($security_level, $outlen_bytes, char[] data, char[]... extras, String optional_customization = "")
	=> xof($security_level, $outlen_bytes, data, ...extras, optional_customization: optional_customization, $is_xof: false);

macro hash_into($security_level, char[] into, char[] data, char[]... extras, String optional_customization = "")
	=> xof_into($security_level, into, data, ...extras, optional_customization: optional_customization, $is_xof: false);

macro char[*] xof($security_level, $outlen_bytes, char[] data, char[]... extras, String optional_customization = "", bool $is_xof = true)
{
	char [$outlen_bytes] result;
	xof_into($security_level, result[..], data, ...extras, optional_customization: optional_customization, $is_xof: $is_xof);
	return result;
}

macro void xof_into($security_level, char[] into, char[] data, char[]... extras, String optional_customization = "", bool $is_xof = true)
{
	TupleHash{$security_level} c @noinit;
	defer c.wipe();
	c.init(optional_customization, $is_xof);
	c.update(data, ...extras);
	c.final(into);
}

// The XOF variant of TupleHash uses a `right_encode(0)` at the end of the input stream.
//   This causes the output value to be completely different and not dependent on the length of the output buffer.
//   To better understand this, or to see the reference, search for NIST SP 800-185.
struct TupleHashXOF @generic(SECURITY_LEVEL)
{
	inline TupleHash{SECURITY_LEVEL} t;
}

fn void TupleHashXOF.init(&self, String optional_customization = "") @inline => self.t.init(optional_customization, true);

struct TupleHash @generic(SECURITY_LEVEL)
{
	inline CShake{SECURITY_LEVEL} c;
	bool is_xof_mode;
}

fn void TupleHash.init(&self, String optional_customization = "", bool is_xof_mode = false)
{
	self.is_xof_mode = is_xof_mode;
	self.c.init(TUPLE_HASH, optional_customization);
}

// This is the more distinct feature of TupleHash: inputs are domain-separated.
//
// So, calling `th.update("abc")` then `th.update("def")` produces a ***DIFFERENT HASH***
//   than calling `th.update("ab")` then `th.update("cdef")`.
//
fn void TupleHash.update(&self, char[] data, char[]... extra)
{
	self.c.update(nist::encode_length(NistEncodingType.LEFT, data.len));
	self.c.update(data);

	foreach (v : extra)
	{
		self.c.update(nist::encode_length(NistEncodingType.LEFT, v.len));
		self.c.update(v);
	}
}

<*
 @require out.len > 0
*>
macro void TupleHash.final(&self, char[] out)
{
	if (self.is_xof_mode)
	{
		self.c.update(nist::@ct_encode_length(NistEncodingType.RIGHT, 0));
	}
	else
	{
		self.c.update(nist::encode_length(NistEncodingType.RIGHT, out.len));
	}

	self.c.final(out);
}
