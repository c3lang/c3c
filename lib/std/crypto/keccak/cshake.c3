// Copyright (c) 2025-2026 Zack Puhl <github@xmit.xyz>. All rights reserved.
// Use of this source code is governed by the MIT license
// a copy of which can be found in the LICENSE_STDLIB file.
//
// See: NIST SP 800-185: https://nvlpubs.nist.gov/nistpubs/SpecialPublications/NIST.SP.800-185.pdf
//
module std::crypto::cshake;

import std::crypto @public;

<* Bubble up the SHAKE usable security levels. *>
const USABLE_LEVELS = shake::USABLE_LEVELS;

<* The Keccak domain-separation delimiter to use specifically for cSHAKE. *>
const DEFAULT_DELIMITER = 0x04;
<* The amount of Keccak rounds to use specifically for cSHAKE. *>
const DEFAULT_ROUNDS = shake::DEFAULT_ROUNDS;

<*
 Generate and return an XOF result of the specified `$outlen_bytes`.

 @param $security_level : "The security level of the XOF to use when hashing."
 @param $outlen_bytes : "The length of the XOF hash to return,"
 @param [in] data : "The data to hash."
 @param optional_customization : "An optional, variable-length customization string used for further domain separation."
 @param $function : "A compile-time string representing the type of NIST hash function."
 @param $rounds : "An optional compile-time override of the rounds count used in hash computation."
 @param $delimiter : "An optional compile-time override of the delimiter used in hash computation."

 @require @in($security_level, ...USABLE_LEVELS) : "Shake structures must use a valid security level."
 @require $outlen_bytes > 0 : "You must use an output length over zero bytes."

 @return "A character array of the specified `$outlen_bytes` size containing the XOF result."
*>
macro char[*] xof($security_level, $outlen_bytes, char[] data, String optional_customization = "",
	NistFunctionName $function = NONE, $rounds = DEFAULT_ROUNDS, $delimiter = DEFAULT_DELIMITER)
{
	char[$outlen_bytes] into @noinit;
	xof_into($security_level, into[..], data, optional_customization, $function, $rounds, $delimiter);
	return into;
}

<*
 Generate an XOF result of `into.len` bytes and store the result in the `into` slice.

 @param $security_level : "The security level of the XOF to use when hashing."
 @param [out] into : "The storage destination for the XOF result. Its length determines the amount of XOF bytes yielded."
 @param [in] data : "The data to hash."
 @param optional_customization : "An optional, variable-length customization string used for further domain separation."
 @param $function : "A compile-time string representing the type of NIST hash function."
 @param $rounds : "An optional compile-time override of the rounds count used in hash computation."
 @param $delimiter : "An optional compile-time override of the delimiter used in hash computation."

 @require @in($security_level, ...USABLE_LEVELS) : "Shake structures must use a valid security level."
 @require into.len > 0 : "You must use an output length over zero bytes."
*>
macro void xof_into($security_level, char[] into, char[] data, String optional_customization = "",
	NistFunctionName $function = NONE, $rounds = DEFAULT_ROUNDS, $delimiter = DEFAULT_DELIMITER)
{
	if (optional_customization == "" && $function == NONE)
	{
		shake::xof_into($security_level, into, data);   // use default SHAKE delimiter, NOT this function's (NIST spec)
		return;
	}
	CShake{$security_level} c;
	c.init($function, optional_customization, $rounds, $delimiter);
	c.update(data);
	c.final(into);
}

<*
 A struct wrapping the SHAKE construct. Maintained as its own full structure for member flexibility.

 @require @in(SECURITY_LEVEL, ...USABLE_LEVELS) : "Shake structures must use a valid security level."
*>
struct CShake @generic(SECURITY_LEVEL)
{
	inline Shake{SECURITY_LEVEL} under;
}

<*
 Initialize the cSHAKE context.
*>
fn void CShake.init(&self, NistFunctionName function = NONE, String optional_customization = "",
	char runtime_rounds = DEFAULT_ROUNDS, char runtime_delimiter = DEFAULT_DELIMITER)
{
	keccak::@init_runtime_size(self, $sizeof(*self));
	String function_name = function.customizer;

	if (optional_customization == "" && function_name == "")
	{
		// override delimiter with default SHAKE delimiter (see NIST spec)
		return self.under.init(runtime_rounds, shake::DEFAULT_DELIMITER);
	}

	self.under.init(runtime_rounds, runtime_delimiter);

	// Automatically encode and update the underlying SHAKE context with the rate length, N (func_name), and S (customization string) values.
	self.under.update(nist::encoded_rate_bytes(SECURITY_LEVEL));

	if (function_name == "")
	{
		self.under.update(nist::ZERO_LEFT);   // encoded zero value (no/empty length)
	}
	else
	{
		self.under.update(nist::encode_length(LEFT, function_name.len));
		self.under.update(function_name[..]);
	}

	if (optional_customization == "")
	{
		self.under.update(nist::ZERO_LEFT);   // encoded zero value (no/empty length)
	}
	else
	{
		self.under.update(nist::encode_length(LEFT, optional_customization.len));
		self.under.update(optional_customization);
	}

	// Fill block and be done.
	self.under.fill_block();
}
