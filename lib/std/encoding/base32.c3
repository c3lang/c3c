module std::encoding::base32;

// This module implements base32 encoding according to RFC 4648
// (https://www.rfc-editor.org/rfc/rfc4648)

distinct Alphabet = inline char[32];

// Standard base32 Alphabet
const Alphabet STD_ALPHABET = "ABCDEFGHIJKLMNOPQRSTUVWXYZ234567";

// Extended Hex Alphabet
const Alphabet HEX_ALPHABET = "0123456789ABCDEFGHIJKLMNOPQRSTUV";

const uint MASK @private = 0b11111;
const char INVALID @private = 0xff;

const int STD_PADDING = '=';
const int NO_PADDING = -1;

fault Base32Error
{
	DUPLICATE_IN_ALPHABET,
	PADDING_IN_ALPHABET,
	INVALID_CHARACTER_IN_ALPHABET,
	DESTINATION_TOO_SMALL,
	INVALID_PADDING,
	CORRUPT_INPUT
}

struct Base32Encoder
{
	Alphabet alphabet;
	int padding;
}

<*
 @param encoder "The 32-character alphabet for encoding."
 @param padding "Set to a negative value to disable padding."
 @require padding < 256
*>
fn void! Base32Encoder.init(&self, Alphabet encoder = STD_ALPHABET, int padding = STD_PADDING)
{
	encoder.validate(padding)!;
	*self = { .alphabet = encoder, .padding = padding };
}

<*
 Calculate the length in bytes of the encoded data.
 @param n "Length in bytes on input."
 @return "Length in bytes of the encoded data."
*>
fn usz Base32Encoder.encode_len(&self, usz n)
{
	// A character is encoded into 8 x 5-bit blocks.
	if (self.padding >= 0)
	{
		// with padding
		return (n + 4) / 5 * 8;
	}
	else
	{
		// no padding
		usz trailing = n % 5;
		return n / 5 * 8 + (trailing * 8 + 4) / 5;
	}
}

<*
 Encode the content of src into dst, which must be properly sized.
 @param [in] src "The input to be encoded."
 @param [inout] dst "The encoded input."
 @return "The encoded size."
 @return! Base32Error.DESTINATION_TOO_SMALL
*>
fn usz! Base32Encoder.encode(&self, char[] src, char[] dst)
{
	if (src.len == 0) return 0;

	usz n = (src.len / 5) * 5;
	usz dn = self.encode_len(src.len);
	if (dst.len < dn) return Base32Error.DESTINATION_TOO_SMALL?;

	uint msb, lsb;
	for (usz i = 0; i < n; i += 5)
	{
		// to fit 40 bits we need two 32-bit uints
		msb = (uint)src[i] << 24 | (uint)src[i+1] << 16
				| (uint)src[i+2] << 8 | (uint)src[i+3];
		lsb = msb << 8 | (uint)src[i+4];

		// now slice them into 5-bit chunks and translate to the
		// alphabet.
		dst[0] = self.alphabet[(msb >> 27) & MASK];
		dst[1] = self.alphabet[(msb >> 22) & MASK];
		dst[2] = self.alphabet[(msb >> 17) & MASK];
		dst[3] = self.alphabet[(msb >> 12) & MASK];
		dst[4] = self.alphabet[(msb >> 7) & MASK];
		dst[5] = self.alphabet[(msb >> 2) & MASK];
		dst[6] = self.alphabet[(lsb >> 5) & MASK];
		dst[7] = self.alphabet[lsb & MASK];

		dst = dst[8..];
	}

	usz trailing = src.len - n;
	if (trailing == 0) return dn;

	msb = 0;
	switch (trailing)
	{
		case 4:
			msb |= (uint)src[n+3];
			lsb = msb << 8;
			dst[6] = self.alphabet[(lsb >> 5) & MASK];
			dst[5] = self.alphabet[(msb >> 2) & MASK];
			nextcase 3;
		case 3:
			msb |= (uint)src[n+2] << 8;
			dst[4] = self.alphabet[(msb >> 7) & MASK];
			nextcase 2;
		case 2:
			msb |= (uint)src[n+1] << 16;
			dst[3] = self.alphabet[(msb >> 12) & MASK];
			dst[2] = self.alphabet[(msb >> 17) & MASK];
			nextcase 1;
		case 1:
			msb |= (uint)src[n] << 24;
			dst[1] = self.alphabet[(msb >> 22) & MASK];
			dst[0] = self.alphabet[(msb >> 27) & MASK];
	}

	// add the padding
	if (self.padding >= 0)
	{
		char pad = (char)self.padding;
		for (usz i = (trailing * 8 / 5) + 1; i < 8; i++)
		{
			dst[i] = pad;
		}
	}

	return dn;
}

struct Base32Decoder
{
	Alphabet alphabet;
	int padding;
	char[256] reverse;
}

<*
 @param decoder "The alphabet used for decoding."
 @param padding "Set to a negative value to disable padding."
 @require padding < 256
*>
fn void! Base32Decoder.init(&self, Alphabet decoder = STD_ALPHABET, int padding = STD_PADDING)
{
	decoder.validate(padding)!;
	*self = { .alphabet = decoder, .padding = padding };

	self.reverse[..] = INVALID;
	foreach (char i, c : decoder)
	{
		self.reverse[c] = i;
	}
}

<*
 Calculate the length in bytes of the decoded data.
 @param n "Length in bytes of input."
 @return "Length in bytes of the decoded data."
*>
fn usz Base32Decoder.decode_len(&self, usz n)
{
	if (self.padding >= 0)
	{
		// with padding
		return (n / 8) * 5;
	}
	else
	{
		// no padding
		usz trailing = n % 8;
		return n / 8 * 5 + (trailing * 5 ) / 8;
	}
}

<*
 Decode the content of src into dst, which must be properly sized.
 @param src "The input to be decoded."
 @param dst "The decoded input."
 @return "The decoded size."
 @return! Base32Error.DESTINATION_TOO_SMALL, Base32Error.CORRUPT_INPUT
*>
fn usz! Base32Decoder.decode(&self, char[] src, char[] dst)
{
	if (src.len == 0) return 0;
	usz dn = self.decode_len(src.len);
	if (dst.len < dn) return Base32Error.DESTINATION_TOO_SMALL?;

	usz j, n;
	char[8] buf;
	while (src.len > 0 && dst.len > 0)
	{

		// load 8 bytes into buffer
		for (j = 0; j < 8; j++)
		{
			if (src.len == 0)
			{
				if (self.padding >= 0)
				{
					return Base32Error.CORRUPT_INPUT?;
				}
				break;
			}
			if (src[0] == (char)self.padding)
			{
				break;
			}
			buf[j] = self.reverse[src[0]];
			if (buf[j] == INVALID)
			{
				return Base32Error.CORRUPT_INPUT?;
			}
			src = src[1..];
		}

		// extract 5-bytes from the buffer which contains 8 x 5 bit chunks
		switch (j)
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
				return Base32Error.CORRUPT_INPUT?;
		}

		if (dst.len < 5) break;
		dst = dst[5..];
	}

	return n;
}


// Validate the 32-character alphabet to make sure that no character occurs
// twice and that the padding is not present in the alphabet.
fn void! Alphabet.validate(&self, int padding)
{
	bool[256] checked;
	foreach (c : self)
	{
		if (checked[c])
		{
			return Base32Error.DUPLICATE_IN_ALPHABET?;
		}
		checked[c] = true;
		if (c == '\r' || c == '\n')
		{
			return Base32Error.INVALID_CHARACTER_IN_ALPHABET?;
		}
	}
	if (padding >= 0)
	{
		char pad = (char)padding;
		if (pad == '\r' || pad == '\n')
		{
			return Base32Error.INVALID_PADDING?;
		}
		if (checked[pad])
		{
			return Base32Error.PADDING_IN_ALPHABET?;
		}
	}
}
