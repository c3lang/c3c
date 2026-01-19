// Copyright (c) 2025-2026 Zack Puhl <github@xmit.xyz>. All rights reserved.
// Use of this source code is governed by the MIT license
// a copy of which can be found in the LICENSE_STDLIB file.
module std::crypto::sha3;

import std::crypto @public;

<* Used for creating SHA-3 HMACs with the "legacy" (i.e., non-KMAC) construction. *>
const char IPAD @local = 0x36;
<* Used for creating SHA-3 HMACs with the "legacy" (i.e., non-KMAC) construction. *>
const char OPAD @local = 0x5c;

<* Valid hash digest sizes; transparently equates to the valid Keccak security levels. *>
const usz[] DIGEST_SIZES = keccak::SECURITY_LEVELS;

<* The Keccak domain-separation delimiter to use specifically for SHA-3. *>
const DEFAULT_DELIMITER = 0x06;

<*
 Generate and return a SHA-3 hash. The return byte array's length is dependent upon the `$hash_bits` used.

 @param $hash_bits : "The strength of the hash function to use. Also determines the fixed output size."
 @param [in] data : "The data to hash."

 @require @in($hash_bits, ...DIGEST_SIZES) : "You must use a valid SHA-3 digest size."

 @return "An array of `$outlen_bytes` bytes containing the hash."
*>
macro char[*] hash($hash_bits, char[] data)
{
	char[$hash_bits / 8] result @noinit;
	hash_into($hash_bits, result[..], data);
	return result;
}

<*
 Generate a SHA-3 hash into a buffer. The length of the `into` buffer determines the resultant size of the hash.
 Note that changes to the requested output length _does not_ affect the contents of the returned hash.

 @param $hash_bits : "The strength of the hash function to use. Also determines the fixed output size."
 @param [out] into : "The destination slice for the hash result."
 @param [in] data : "The data to hash."

 @require @in($hash_bits, ...DIGEST_SIZES) : "You must use a valid SHA-3 digest size."
 @require into.len > 0 : "The destination result slice must have a length greater than 0."
*>
macro void hash_into($hash_bits, char[] into, char[] data)
{
	Sha3{$hash_bits} s;
	s.init();
	s.update(data);
	s.final(into);
}

<*
 SHA-3 is not vulnerable to length extension attacks; therefore, `SHA-3(key || message)` is
 valid and doesn't require the nested HMAC construction.

 **WARNING**: This is not an 'official' NIST HMAC, and it's not likely to be used widely. You should
 NOT use this if you want to form SHA-3 HMACs that will validate with non-C3 implementations of SHA-3.
 If that's your requirement, you should use the plain `hmac` construction instead.

 @param $hash_bits : "The strength of the hash function to use. Also determines the fixed output size."
 @param [in] key : "The secrete key used to authenticate the data."
 @param [in] data : "The data to authenticate."

 @require @in($hash_bits, ...DIGEST_SIZES) : "You must use a valid SHA-3 digest size."

 @return "An array of `$outlen_bytes` bytes containing the hash."
*>
macro char[*] simple_hmac($hash_bits, char[] key, char[] data)
{
	char[$hash_bits / 8] result @noinit;
	simple_hmac_into($hash_bits, result[..], key, data);
	return result;
}

<*

 SHA-3 is not vulnerable to length extension attacks; therefore, `SHA-3(key || message)` is
 valid and doesn't require the nested HMAC construction.

 **WARNING**: This is not an 'official' NIST HMAC, and it's not likely to be used widely. You should
 NOT use this if you want to form SHA-3 HMACs that will validate with non-C3 implementations of SHA-3.
 If that's your requirement, you should use the plain `hmac_into` construction instead.

 @param $hash_bits : "The strength of the hash function to use. Also determines the fixed output size."
 @param [out] into : "The destination slice for the hash result."
 @param [in] key : "The secrete key used to authenticate the data."
 @param [in] data : "The data to authenticate."

 @require @in($hash_bits, ...DIGEST_SIZES) : "You must use a valid SHA-3 digest size."
 @require into.len > 0 : "You must use an output length over zero bytes."
*>
macro void simple_hmac_into($hash_bits, char[] into, char[] key, char[] data)
{
	Sha3{$hash_bits} s;
	s.init();
	s.update(key);
	s.update(data);
	s.final(into);
}

<*
 While SHA-3 is not vulnerable to length extension attacks, and `SHA-3(key || message)` works
 just fine, many libraries will still use the old nested construction from SHA-2. This carries
 that implementation over, but also allows one to specify a "tag size", which is the desired
 length of the resulting hash (i.e., 'squeezed' at that length from the Keccak sponge).

 The ordinary std::hash::hmac module is NOT used for this, because we can specify a "tag size"
 with this macro to get a variable-length MAC.

 Formula: `H((K' ^ opad) || H((K' ^ ipad) || m))`, where `K' = K.len > block_size ? H(K) : K`

 @param $hash_bits : "The strength of the hash function to use. Does NOT determine the output size."
 @param $tag_size : "The size, in bytes, of the resulting MAC byte array."
 @param [in] key : "The secret key used to authenticate the data."
 @param [in] data : "The data to use when creating the HMAC."

 @require @in($hash_bits, ...DIGEST_SIZES) : "You must use a valid SHA-3 digest size."
 @require $tag_size > 0 : "You must use a tag size over zero bytes."

 @return "An array of `$outlen_bytes` bytes containing the hash."
*>
macro char[*] hmac($hash_bits, usz $tag_size, char[] key, char[] data)
{
	char[$tag_size] result @noinit;
	hmac_into($hash_bits, result[..], key, data);
	return result;
}

<*
 While SHA-3 is not vulnerable to length extension attacks, and `SHA-3(key || message)` works
 just fine, many libraries will still use the old nested construction from SHA-2. This carries
 that implementation over, but also allows one to specify a "tag size", which is the desired
 length of the resulting hash (i.e., 'squeezed' at that length from the Keccak sponge).

 The ordinary std::hash::hmac module is NOT used for this, because we can specify a "tag size"
 with this macro to get a variable-length MAC.

 Formula: `H((K' ^ opad) || H((K' ^ ipad) || m))`, where `K' = K.len > block_size ? H(K) : K`

 @param $hash_bits : "The strength of the hash function to use. Does NOT determine the output size."
 @param [out] into : "The destination slice for the hash result. The size of this slice determined the resultant tag length."
 @param [in] key : "The secret key used to authenticate the data."
 @param [in] data : "The data to use when creating the HMAC."

 @require @in($hash_bits, ...DIGEST_SIZES) : "You must use a valid SHA-3 digest size."
 @require into.len > 0 : "You must use an output length over zero bytes."
*>
macro void hmac_into($hash_bits, char[] into, char[] key, char[] data)
{
	usz $block_size = keccak::rate($hash_bits);
	char[$block_size] k_prime;
	if (key.len > $block_size)
	{
		hash_into($hash_bits, k_prime[:$hash_bits / 8], key);
	}
	else
	{
		k_prime[:key.len] = key[..];
	}
	char[$block_size] _ipad @noinit; _ipad[..] = IPAD; array::@zip_into(_ipad[..], k_prime, fn (a, b) => a ^ b);
	char[$block_size] _opad @noinit; _opad[..] = OPAD; array::@zip_into(_opad[..], k_prime, fn (a, b) => a ^ b);

	char[$hash_bits / 8] intermediate_ipad = simple_hmac($hash_bits, _ipad[..], data);
	simple_hmac_into($hash_bits, into, _opad[..], intermediate_ipad[..]);
}

<*
 A struct wrapping the base Keccak construct. Maintained as its own full structure for member flexibility.

 @require @in(HASH_BITS, ...DIGEST_SIZES) : "You must use a valid SHA-3 digest size."
*>
struct Sha3 <HASH_BITS>
{
	inline Keccak{HASH_BITS} k;
}

<*
 Initialize the inner Keccak context with the SHA-3 domain-separation delimiter.
*>
fn void Sha3.init(&self)
{
	keccak::@init_runtime_size(self, $sizeof(*self));
	self.k.init(runtime_delimiter: DEFAULT_DELIMITER);
}
