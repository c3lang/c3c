// Copyright (c) 2025-2026 Zack Puhl <github@xmit.xyz>. All rights reserved.
// Use of this source code is governed by the MIT license
// a copy of which can be found in the LICENSE_STDLIB file.
//
// See: NIST SP 800-185: https://nvlpubs.nist.gov/nistpubs/SpecialPublications/NIST.SP.800-185.pdf
//
module std::hash::sha3::cshake;

import std::hash::sha3::keccak;
import std::hash::sha3::nist @public;
import std::hash::sha3::shake;

const DEFAULT_DELIMITER = 0x04;
const DEFAULT_ROUNDS = shake::DEFAULT_ROUNDS;

macro char[*] xof($security_level, $outlen_bytes, char[] data, String optional_customization = "",
	NistFunctionName $function = NONE, $rounds = DEFAULT_ROUNDS, $delimiter = DEFAULT_DELIMITER)
{
	char[$outlen_bytes] into;
	xof_into($security_level, into[..], data, optional_customization, $function, $rounds, $delimiter);
	return into;
}

macro void xof_into($security_level, char[] into, char[] data, String optional_customization = "",
	NistFunctionName $function = NONE, $rounds = DEFAULT_ROUNDS, $delimiter = DEFAULT_DELIMITER)
{
	if (optional_customization == "" && $function == NONE)
	{
		shake::xof_into($security_level, into, data);   // use default SHAKE delimiter, NOT this function's (NIST spec)
		return;
	}
	CShake{$security_level} c @noinit;
	defer c.wipe();
	c.init($function, optional_customization, $rounds, $delimiter);
	c.update(data);
	c.final(into);
}

struct CShake @generic(SECURITY_LEVEL)
{
	inline Shake{SECURITY_LEVEL} under;
}

macro void CShake.init(&self, NistFunctionName $function = NONE, String optional_customization = "",
	char runtime_rounds = DEFAULT_ROUNDS, char runtime_delimiter = DEFAULT_DELIMITER)
{
	mem::zero_volatile(@as_char_view(*self));

	String $function_name = $function.customizer;

	if (optional_customization == "" && $function_name == "")
	{
		return self.under.init(runtime_rounds, shake::DEFAULT_DELIMITER);   // override delimiter with default SHAKE delimiter (see NIST spec)
	}

	self.under.init(runtime_rounds, runtime_delimiter);

	// Automatically encode and update the underlying SHAKE context with the rate length, N (func_name), and S (customization string) values.
	self.under.update(nist::@ct_encode_length(NistEncodingType.LEFT, keccak::rate(SECURITY_LEVEL) / 8));

	$if $function_name == "":
		self.under.update(nist::@ct_encode_length(NistEncodingType.LEFT, 0));   // encoded zero value (no/empty length)
	$else
		self.under.update(nist::@ct_encode_length(NistEncodingType.LEFT, $function_name.len));
		self.under.update($function_name[..]);
	$endif

	if (optional_customization == "")
	{
		self.under.update(nist::@ct_encode_length(NistEncodingType.LEFT, 0));   // encoded zero value (no/empty length)
	}
	else
	{
		self.under.update(nist::encode_length(NistEncodingType.LEFT, optional_customization.len));
		self.under.update(optional_customization);
	}

	// Fill block and be done.
	self.under.fill_block();
}
