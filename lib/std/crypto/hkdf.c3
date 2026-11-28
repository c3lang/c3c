module std::crypto::hkdf <HashAlg, HASH_BYTES, BLOCK_BYTES>;
import std::hash::hmac;

<*
 HKDF-Extract (RFC 5869 section 2.2). An empty salt acts as HASH_BYTES zero octets.

 @param [in] ikm : "Input keying material"
 @param [in] salt : "Optional salt, any length"
 @return "The pseudorandom key (PRK)"
*>
fn char[HASH_BYTES] extract(char[] ikm, char[] salt = { })
{
	Hmac{HashAlg, HASH_BYTES, BLOCK_BYTES} hmac @noinit;
	char[HASH_BYTES] zero_salt;

	hmac.init(salt.len == 0 ? zero_salt[..] : salt);
	hmac.update(ikm);
	return hmac.final();
}

<*
 HKDF-Expand (RFC 5869 section 2.3). Fills `okm` entirely.

 `prk` should come from `extract`, or be an already uniformly random key (RFC 5869 section 3.3).

 @param [in] prk : "Pseudorandom key, at least HASH_BYTES long"
 @param [in] info : "Optional context and application specific info"
 @param [out] okm : "Output keying material"

 @require prk.len >= HASH_BYTES : "PRK must be at least one hash output long"
 @require okm.len <= 255 * HASH_BYTES : "Output is too large"
 @require okm.ptr + okm.len <= prk.ptr || prk.ptr + prk.len <= okm.ptr : "okm must not overlap prk"
 @require okm.ptr + okm.len <= info.ptr || info.ptr + info.len <= okm.ptr : "okm must not overlap info"
*>
fn void expand(char[] prk, char[] info, char[] okm)
{
	sz n = (okm.len + HASH_BYTES - 1) / HASH_BYTES;
	char[HASH_BYTES] previous_t;
	defer mem::zero_volatile(&previous_t);

	sz offset = 0;
	Hmac{HashAlg, HASH_BYTES, BLOCK_BYTES} keyed @noinit;
	keyed.init(prk);

	for (sz i = 1; i <= n; i++)
	{
		Hmac{HashAlg, HASH_BYTES, BLOCK_BYTES} hmac = keyed;
		if (i > 1) hmac.update(&previous_t);

		hmac.update(info);
		char counter = (char)i;
		hmac.update((&counter)[:1]);
		previous_t = hmac.final();

		sz remaining = okm.len - offset;
		sz copy_len = remaining < HASH_BYTES ? remaining : HASH_BYTES;

		okm[offset:copy_len] = previous_t[:copy_len];
		offset += copy_len;
	}
}

<*
 HKDF (RFC 5869): Extract then Expand. Zeroes the intermediate PRK before returning.

 @param [in] ikm : "Input keying material"
 @param [in] salt : "Optional salt, any length"
 @param [in] info : "Optional context and application specific info"
 @param [out] okm : "Output keying material"

 @require okm.len <= 255 * HASH_BYTES : "Output is too large"
*>
fn void derive(char[] ikm, char[] salt, char[] info, char[] okm)
{
	char[HASH_BYTES] prk = extract(ikm, salt);
	defer mem::zero_volatile(&prk);
	expand(&prk, info, okm);
}
