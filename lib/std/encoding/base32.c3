module std::encoding::base32;

// This module implements base32 encoding according to RFC 4648
// (https://www.rfc-editor.org/rfc/rfc4648)

struct Base32Alphabet
{
	char[32] encoding;
	char[256] reverse;
}

const char NO_PAD = 0;
const char DEFAULT_PAD = '=';

<*
 Encode the content of src into a newly allocated string
 @param [in] src : "The input to be encoded."
 @param padding : "The padding character or 0 if none"
 @param alphabet : "The alphabet to use"
 @require padding < 0xFF : "Invalid padding character"
 @return "The encoded string."
*>
fn String? encode(Allocator allocator, char[] src, char padding = DEFAULT_PAD, Base32Alphabet* alphabet = &STANDARD)
{
	char[] dst = allocator::alloc_array(allocator, char, encode_len(src.len, padding));
	return encode_buffer(src, dst, padding, alphabet);
}

<*
 Decode the content of src into a newly allocated char array.
 @param [in] src : "The input to be encoded."
 @param padding : "The padding character or 0 if none"
 @param alphabet : "The alphabet to use"
 @require padding < 0xFF : "Invalid padding character"
 @return "The decoded data."
*>
fn char[]? decode(Allocator allocator, char[] src, char padding = DEFAULT_PAD, Base32Alphabet* alphabet = &STANDARD)
{
	char[] dst = allocator::alloc_array(allocator, char, decode_len(src.len, padding));
	return decode_buffer(src, dst, padding, alphabet);
}

fn String? tencode(char[] code, char padding = DEFAULT_PAD, Base32Alphabet* alphabet = &STANDARD) @inline => encode(tmem, code, padding, alphabet);
fn char[]? tdecode(char[] code, char padding = DEFAULT_PAD, Base32Alphabet* alphabet = &STANDARD) @inline => decode(tmem, code, padding, alphabet);

<*
 Calculate the length in bytes of the decoded data.
 @param n : "Length in bytes of input."
 @param padding : "The padding character or 0 if none"
 @require padding < 0xFF : "Invalid padding character"
 @return "Length in bytes of the decoded data."
*>
fn usz decode_len(usz n, char padding)
{
	if (padding) return (n / 8) * 5;
	// no padding
	usz trailing = n % 8;
	return n / 8 * 5 + (trailing * 5 ) / 8;
}

<*
 Calculate the length in bytes of the encoded data.
 @param n : "Length in bytes on input."
 @param padding : "The padding character or 0 if none"
 @require padding < 0xFF : "Invalid padding character"
 @return "Length in bytes of the encoded data."
*>
fn usz encode_len(usz n, char padding)
{
	// A character is encoded into 8 x 5-bit blocks.
	if (padding) return (n + 4) / 5 * 8;

	// no padding
	usz trailing = n % 5;
	return n / 5 * 8 + (trailing * 8 + 4) / 5;
}

<*
 Decode the content of src into dst, which must be properly sized.
 @param src : "The input to be decoded."
 @param dst : "The decoded input."
 @param padding : "The padding character or 0 if none"
 @param alphabet : "The alphabet to use"
 @require padding < 0xFF : "Invalid padding character"
 @require dst.len >= decode_len(src.len, padding) : "Destination buffer too small"
 @return "The resulting dst buffer"
 @return? encoding::INVALID_PADDING, encoding::INVALID_CHARACTER
*>
fn char[]? decode_buffer(char[] src, char[] dst, char padding = DEFAULT_PAD, Base32Alphabet* alphabet = &STANDARD)
{
	if (src.len == 0) return dst[:0];
	char* dst_ptr = dst;
	usz dn = decode_len(src.len, padding);
	usz n;
	char[8] buf;
	while (src.len > 0 && dst.len > 0)
	{
		usz i @noinit;
		// load 8 bytes into buffer
		for (i = 0; i < 8; i++)
		{
			if (src.len == 0)
			{
				if (padding > 0) return encoding::INVALID_PADDING?;
				break;
			}
			if (src[0] == padding) break;
			buf[i] = alphabet.reverse[src[0]];
			if (buf[i] == INVALID) return encoding::INVALID_CHARACTER?;
			src = src[1..];
		}

		// extract 5-bytes from the buffer which contains 8 x 5 bit chunks
		switch (i)
		{
			case 8:
				// |66677777|  dst[4]
				// |   77777|  buf[7]
				// |666     |  buf[6] << 5
				dst[4] = buf[7] | buf[6] << 5;
				n++;
				nextcase 7;
			case 7:
				// |45555566|  dst[3]
				// |      66|  buf[6] >> 3
				// | 55555  |  buf[5] << 2
				// |4       |  buf[4] << 7
				dst[3] = buf[6] >> 3 | buf[5] << 2 | buf[4] << 7;
				n++;
				nextcase 5;
			case 5:
				// |33334444|  dst[2]
				// |    4444|  buf[4] >> 1
				// |3333    |  buf[3] << 4
				dst[2] = buf[4] >> 1 | buf[3] << 4;
				n++;
				nextcase 4;
			case 4:
				// |11222223|  dst[1]
				// |       3|  buf[3] >> 4
				// |  22222 |  buf[2] << 1
				// |11      |  buf[1] << 6
				dst[1] = buf[3] >> 4 | buf[2] << 1 | buf[1] << 6;
				n++;
				nextcase 2;
			case 2:
				// |00000111|  dst[0]
				// |     111|  buf[1] >> 2
				// |00000   |  buf[0] << 3
				dst[0] = buf[1] >> 2 | buf[0] << 3;
				n++;
			default:
				return encoding::INVALID_CHARACTER?;
		}
		if (dst.len < 5) break;
		dst = dst[5..];
	}
	return dst_ptr[:n];
}

<*
 Encode the content of src into dst, which must be properly sized.
 @param [in] src : "The input to be encoded."
 @param [inout] dst : "The encoded input."
 @param padding : "The padding character or 0 if none"
 @param alphabet : "The alphabet to use"
 @require padding < 0xFF : "Invalid padding character"
 @require dst.len >= encode_len(src.len, padding) : "Destination buffer too small"
 @return "The encoded size."
*>
fn String encode_buffer(char[] src, char[] dst, char padding = DEFAULT_PAD, Base32Alphabet* alphabet = &STANDARD)
{
	if (src.len == 0) return (String)dst[:0];

	char* dst_ptr = dst;
	usz n = (src.len / 5) * 5;
	usz dn = encode_len(src.len, padding);

	uint msb, lsb;
	for (usz i = 0; i < n; i += 5)
	{
		// to fit 40 bits we need two 32-bit uints
		msb = (uint)src[i] << 24 | (uint)src[i+1] << 16
				| (uint)src[i+2] << 8 | (uint)src[i+3];
		lsb = msb << 8 | (uint)src[i+4];

		// now slice them into 5-bit chunks and translate to the
		// alphabet.
		dst[0] = alphabet.encoding[(msb >> 27) & MASK];
		dst[1] = alphabet.encoding[(msb >> 22) & MASK];
		dst[2] = alphabet.encoding[(msb >> 17) & MASK];
		dst[3] = alphabet.encoding[(msb >> 12) & MASK];
		dst[4] = alphabet.encoding[(msb >> 7) & MASK];
		dst[5] = alphabet.encoding[(msb >> 2) & MASK];
		dst[6] = alphabet.encoding[(lsb >> 5) & MASK];
		dst[7] = alphabet.encoding[lsb & MASK];

		dst = dst[8..];
	}

	usz trailing = src.len - n;
	if (trailing == 0) return (String)dst_ptr[:dn];

	msb = 0;
	switch (trailing)
	{
		case 4:
			msb |= (uint)src[n+3];
			lsb = msb << 8;
			dst[6] = alphabet.encoding[(lsb >> 5) & MASK];
			dst[5] = alphabet.encoding[(msb >> 2) & MASK];
			nextcase 3;
		case 3:
			msb |= (uint)src[n+2] << 8;
			dst[4] = alphabet.encoding[(msb >> 7) & MASK];
			nextcase 2;
		case 2:
			msb |= (uint)src[n+1] << 16;
			dst[3] = alphabet.encoding[(msb >> 12) & MASK];
			dst[2] = alphabet.encoding[(msb >> 17) & MASK];
			nextcase 1;
		case 1:
			msb |= (uint)src[n] << 24;
			dst[1] = alphabet.encoding[(msb >> 22) & MASK];
			dst[0] = alphabet.encoding[(msb >> 27) & MASK];
	}

	// add the padding
	if (padding > 0)
	{
		for (usz i = (trailing * 8 / 5) + 1; i < 8; i++)
		{
			dst[i] = padding;
		}
	}
	return (String)dst_ptr[:dn];
}

const uint MASK @private = 0b11111;
const char INVALID @private = 0xff;

const int STD_PADDING = '=';
const int NO_PADDING = -1;

typedef Alphabet = char[32];
// Standard base32 Alphabet
const Alphabet STD_ALPHABET = "ABCDEFGHIJKLMNOPQRSTUVWXYZ234567";
// Extended Hex Alphabet
const Alphabet HEX_ALPHABET = "0123456789ABCDEFGHIJKLMNOPQRSTUV";

const Base32Alphabet STANDARD = {
	.encoding = "ABCDEFGHIJKLMNOPQRSTUVWXYZ234567",
	.reverse = x`ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff
                 ffffffffffffffffffffffffffffffffffff1a1b1c1d1e1fffffffffffffffff
                 ff000102030405060708090a0b0c0d0e0f10111213141516171819ffffffffff
                 ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff
                 ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff
                 ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff
                 ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff
                 ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff`
};

const Base32Alphabet HEX = {
	.encoding = "0123456789ABCDEFGHIJKLMNOPQRSTUV",
	.reverse = x`ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff
                 ffffffffffffffffffffffffffffffff00010203040506070809ffffffffffff
                 ff0a0b0c0d0e0f101112131415161718191a1b1c1d1e1fffffffffffffffffff
                 ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff
                 ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff
                 ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff
                 ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff
                 ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff`
};
