module std::encoding::base64;
import std::core::bitorder;

// The implementation is based on https://www.rfc-editor.org/rfc/rfc4648
// Specifically this section:
// https://www.rfc-editor.org/rfc/rfc4648#section-4

const char NO_PAD = 0;
const char DEFAULT_PAD = '=';

struct Base64Alphabet
{
	char[64] encoding;
	char[256] reverse;
}

const Base64Alphabet STANDARD = {
	.encoding = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/",
	.reverse =
	x`ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff
	  ffffffffffffffffffffff3effffff3f3435363738393a3b3c3dffffffffffff
	  ff000102030405060708090a0b0c0d0e0f10111213141516171819ffffffffff
	  ff1a1b1c1d1e1f202122232425262728292a2b2c2d2e2f30313233ffffffffff
	  ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff
	  ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff
	  ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff
	  ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff`
};

const Base64Alphabet URL = {
	.encoding = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789-_",
	.reverse =
	x`ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff
	  ffffffffffffffffffffffffff3effff3435363738393a3b3c3dffffffffffff
	  ff000102030405060708090a0b0c0d0e0f10111213141516171819ffffffff3f
	  ff1a1b1c1d1e1f202122232425262728292a2b2c2d2e2f30313233ffffffffff
	  ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff
	  ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff
	  ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff
	  ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff`
};

const STD_ALPHABET = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/";
const URL_ALPHABET = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789-_";

fn String encode(Allocator allocator, char[] src, char padding = DEFAULT_PAD, Base64Alphabet* alphabet = &STANDARD)
{
	char[] dst = allocator::alloc_array(allocator, char, encode_len(src.len, padding));
	return encode_buffer(src, dst, padding, alphabet);
}

fn char[]? decode(Allocator allocator, char[] src, char padding = DEFAULT_PAD, Base64Alphabet* alphabet = &STANDARD)
{
	char[] dst = allocator::alloc_array(allocator, char, decode_len(src.len, padding))!;
	return decode_buffer(src, dst, padding, alphabet);
}

fn String tencode(char[] code, char padding = DEFAULT_PAD, Base64Alphabet* alphabet = &STANDARD) @inline => encode(tmem, code, padding, alphabet);
fn char[]? tdecode(char[] code, char padding = DEFAULT_PAD, Base64Alphabet* alphabet = &STANDARD) @inline => decode(tmem, code, padding, alphabet);


<*
 Calculate the size of the encoded data.
 @param n : "Size of the input to be encoded."
 @param padding : "The padding character or 0 if none"
 @require padding < 0xFF : "Invalid padding character"
 @return "The size of the input once encoded."
*>
fn usz encode_len(usz n, char padding)
{
	if (padding) return (n + 2) / 3 * 4;
	usz trailing = n % 3;
	return n / 3 * 4 + (trailing * 4 + 2) / 3;
}

<*
 Calculate the size of the decoded data.
 @param n : "Size of the input to be decoded."
 @param padding : "The padding character or 0 if none"
 @require padding < 0xFF : "Invalid padding character"
 @return "The size of the input once decoded."
 @return? encoding::INVALID_PADDING
*>
fn usz? decode_len(usz n, char padding)
{
	usz dn = n / 4 * 3;
	usz trailing = n % 4;
	if (padding)
	{
		if (trailing != 0) return encoding::INVALID_PADDING?;
		// source size is multiple of 4
		return dn;
	}
	if (trailing == 1) return encoding::INVALID_PADDING?;
	return dn + trailing * 3 / 4;
}

<*
 Encode the content of src into dst, which must be properly sized.
 @param src : "The input to be encoded."
 @param dst : "The encoded input."
 @param padding : "The padding character or 0 if none"
 @param alphabet : "The alphabet to use"
 @require padding < 0xFF : "Invalid padding character"
 @return "The encoded size."
*>
fn String encode_buffer(char[] src, char[] dst, char padding = DEFAULT_PAD, Base64Alphabet* alphabet = &STANDARD)
{
	if (src.len == 0) return (String)dst[:0];
	usz dn = encode_len(src.len, padding);
	char* dst_ptr = dst;
	assert(dst.len >= dn);
	usz trailing = src.len % 3;
	char[] src3 = src[:^trailing];

	while (src3.len > 0)
	{
		uint group = (uint)src3[0] << 16 | (uint)src3[1] << 8 | (uint)src3[2];
		dst[0] = alphabet.encoding[group >> 18 & MASK];
		dst[1] = alphabet.encoding[group >> 12 & MASK];
		dst[2] = alphabet.encoding[group >> 6 & MASK];
		dst[3] = alphabet.encoding[group & MASK];
		dst = dst[4..];
		src3 = src3[3..];
	}

	// Encode the remaining bytes according to:
	// https://www.rfc-editor.org/rfc/rfc4648#section-3.5
	switch (trailing)
	{
		case 1:
			uint group = (uint)src[^1] << 16;
			dst[0] = alphabet.encoding[group >> 18 & MASK];
			dst[1] = alphabet.encoding[group >> 12 & MASK];
			if (padding > 0)
			{
				dst[2] = padding;
				dst[3] = padding;
			}
		case 2:
			uint group = (uint)src[^2] << 16 | (uint)src[^1] << 8;
			dst[0] = alphabet.encoding[group >> 18 & MASK];
			dst[1] = alphabet.encoding[group >> 12 & MASK];
			dst[2] = alphabet.encoding[group >> 6 & MASK];
			if (padding > 0)
			{
				dst[3] = padding;
			}
		case 0:
			break;
		default:
			unreachable();
	}
	return (String)dst_ptr[:dn];
}

<*
 Decode the content of src into dst, which must be properly sized.
 @param src : "The input to be decoded."
 @param dst : "The decoded input."
 @param padding : "The padding character or 0 if none"
 @param alphabet : "The alphabet to use"
 @require (decode_len(src.len, padding) ?? 0) <= dst.len : "Destination buffer too small"
 @require padding < 0xFF : "Invalid padding character"
 @return "The decoded data."
 @return? encoding::INVALID_CHARACTER, encoding::INVALID_PADDING
*>
fn char[]? decode_buffer(char[] src, char[] dst, char padding = DEFAULT_PAD, Base64Alphabet* alphabet = &STANDARD)
{
	if (src.len == 0) return dst[:0];
	usz dn = decode_len(src.len, padding)!;
	assert(dst.len >= dn);

	usz trailing = src.len % 4;
	char* dst_ptr = dst;
	char[] src4 = src;
	switch
	{
		case !padding:
			src4 = src[:^trailing];
		default:
			// If there is padding, keep the last 4 bytes for later.
			// NB. src.len >= 4 as decode_len passed
			trailing = 4;
			if (src[^1] == padding) src4 = src[:^4];
	}
	while (src4.len > 0)
	{
		char c0 = alphabet.reverse[src4[0]];
		char c1 = alphabet.reverse[src4[1]];
		char c2 = alphabet.reverse[src4[2]];
		char c3 = alphabet.reverse[src4[3]];
		switch (0xFF)
		{
			case c0:
			case c1:
			case c2:
			case c3:
				return encoding::INVALID_CHARACTER?;
		}
		uint group = (uint)c0 << 18 | (uint)c1 << 12 | (uint)c2 << 6 | (uint)c3;
		dst[0] = (char)(group >> 16);
		dst[1] = (char)(group >> 8);
		dst[2] = (char)group;
		dst = dst[3..];
		src4 = src4[4..];
	}

	if (trailing == 0) return dst_ptr[:dn];

	src = src[^trailing..];
	char c0 = alphabet.reverse[src[0]];
	char c1 = alphabet.reverse[src[1]];
	if (c0 == 0xFF || c1 == 0xFF) return encoding::INVALID_PADDING?;
	if (!padding)
	{
		switch (src.len)
		{
			case 2:
				uint group = (uint)c0 << 18 | (uint)c1 << 12;
				dst[0] = (char)(group >> 16);
			case 3:
				char c2 = alphabet.reverse[src[2]];
				if (c2 == 0xFF) return encoding::INVALID_CHARACTER?;
				uint group = (uint)c0 << 18 | (uint)c1 << 12 | (uint)c2 << 6;
				dst[0] = (char)(group >> 16);
				dst[1] = (char)(group >> 8);
		}
	}
	else
	{
		// Valid paddings are:
		// 2: xx==
		// 1: xxx=
		switch (padding)
		{
			case src[2]:
				if (src[3] != padding) return encoding::INVALID_PADDING?;
				uint group = (uint)c0 << 18 | (uint)c1 << 12;
				dst[0] = (char)(group >> 16);
				dn -= 2;
			case src[3]:
				char c2 = alphabet.reverse[src[2]];
				if (c2 == 0xFF) return encoding::INVALID_CHARACTER?;
				uint group = (uint)c0 << 18 | (uint)c1 << 12 | (uint)c2 << 6;
				dst[0] = (char)(group >> 16);
				dst[1] = (char)(group >> 8);
				dn -= 1;
		}
	}
	return dst_ptr[:dn];
}

const MASK @private = 0b111111;

