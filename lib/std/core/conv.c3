module std::core::string::conv;
import std::bits;

enum UnicodeBom
{
	NO_BOM,
	UTF8,
	UTF16_LE,
	UTF16_BE,
	UTF32_LE,
	UTF32_BE
}
const uint UTF16_SURROGATE_OFFSET @private = 0x10000;
const uint UTF16_SURROGATE_GENERIC_MASK @private = 0xF800;
const uint UTF16_SURROGATE_GENERIC_VALUE @private = 0xD800;
const uint UTF16_SURROGATE_MASK @private = 0xFC00;
const uint UTF16_SURROGATE_CODEPOINT_MASK @private = 0x03FF;
const uint UTF16_SURROGATE_BITS @private = 10;
const uint UTF16_SURROGATE_LOW_VALUE @private = 0xDC00;
const uint UTF16_SURROGATE_HIGH_VALUE @private = 0xD800;

fn UnicodeBom detect_bom(char[] input)
{
	if (input.len < 2) return NO_BOM;
	switch (input)
	{
		case x'FEFF0000': return UTF32_LE;
		case x'FEFF' : return UTF16_BE;
		case x'FFFE' : return UTF16_LE;
		case x'EFBBBF': return UTF8;
		case x'0000FEFF': return UTF32_LE;
		default: return NO_BOM;
	}
}

<*
 @param c : `The utf32 codepoint to convert`
 @param [out] output : `the resulting buffer`
 @return? string::CONVERSION_FAILED
*>
fn sz? char32_to_utf8(Char32 c, char[] output)
{
	if (!output.len) return string::CONVERSION_FAILED~;
	switch (true)
	{
		case c <= 0x7f:
			output[0] = (char)c;
			return 1;
		case c <= 0x7ff:
			if (output.len < 2) return string::CONVERSION_FAILED~;
			output[0] = (char)(0xC0 | c >> 6);
			output[1] = (char)(0x80 | (c & 0x3F));
			return 2;
		case c <= 0xffff:
			if (output.len < 3) return string::CONVERSION_FAILED~;
			output[0] = (char)(0xE0 | c >> 12);
			output[1] = (char)(0x80 | (c >> 6 & 0x3F));
			output[2] = (char)(0x80 | (c & 0x3F));
			return 3;
		case c <= 0x10ffff:
			if (output.len < 4) return string::CONVERSION_FAILED~;
			output[0] = (char)(0xF0 | c >> 18);
			output[1] = (char)(0x80 | (c >> 12 & 0x3F));
			output[2] = (char)(0x80 | (c >> 6 & 0x3F));
			output[3] = (char)(0x80 | (c & 0x3F));
			return 4;
		default:
			// 0x10FFFF and above is not defined.
			return string::CONVERSION_FAILED~;
	}
}

<*
 Convert a code pointer into 1-2 UTF16 characters.

 @param c : `The character to convert.`
 @param [inout] output : `the resulting UTF16 buffer to write to.`
*>
fn void char32_to_utf16_unsafe(Char32 c, Char16** output)
{
	if (c < UTF16_SURROGATE_OFFSET)
	{
		(*output)++[0] = (Char16)c;
		return;
	}
	c -= UTF16_SURROGATE_OFFSET;
	Char16 low = (Char16)(UTF16_SURROGATE_LOW_VALUE | (c & UTF16_SURROGATE_CODEPOINT_MASK));
	c >>= UTF16_SURROGATE_BITS;
	Char16 high = (Char16)(UTF16_SURROGATE_HIGH_VALUE | (c & UTF16_SURROGATE_CODEPOINT_MASK));
	(*output)++[0] = (Char16)high;
	(*output)++[0] = (Char16)low;
}

<*
 Convert 1-2 UTF16 data points into UTF8.

 @param [in] ptr : `The UTF16 data to convert.`
 @param [inout] available : `amount of UTF16 data available.`
 @param [inout] output : `the resulting utf8 buffer to write to.`
*>
macro void? char16_to_utf8_unsafe(Char16 *ptr, sz *available, char** output, bool $unaligned, bool $byteswap)
{
	Char16 high = $unaligned ??? mem::load(ptr, $align:1) : *ptr;
	$if $byteswap:
		high = bswap(high);
	$endif
	if (high & UTF16_SURROGATE_GENERIC_MASK != UTF16_SURROGATE_GENERIC_VALUE)
	{
		char32_to_utf8_unsafe(high, output);
		*available = 1;
		return;
	}
	// Low surrogate first is an error
	if (high & UTF16_SURROGATE_MASK != UTF16_SURROGATE_HIGH_VALUE) return string::INVALID_UTF16~;

	// Unmatched high surrogate is an error
	if (*available == 1) return string::INVALID_UTF16~;

	Char16 low = ptr[1];
	$if $byteswap:
		low = bswap(low);
	$endif
	// Unmatched high surrogate, invalid
	if (low & UTF16_SURROGATE_MASK != UTF16_SURROGATE_LOW_VALUE) return string::INVALID_UTF16~;

	// The high bits of the codepoint are the value bits of the high surrogate
	// The low bits of the codepoint are the value bits of the low surrogate
	Char32 uc = (high & UTF16_SURROGATE_CODEPOINT_MASK) << UTF16_SURROGATE_BITS
				| (low & UTF16_SURROGATE_CODEPOINT_MASK) + UTF16_SURROGATE_OFFSET;
	char32_to_utf8_unsafe(uc, output);
	*available = 2;
}
<*
 @param c : `The utf32 codepoint to convert`
 @param [inout] output : `the resulting buffer`
*>
macro sz char32_to_utf8_unsafe(Char32 c, char** output)
{
	switch
	{
		case c <= 0x7f:
			(*output)++[0] = (char)c;
			return 1;
		case c <= 0x7ff:
			(*output)++[0] = (char)(0xC0 | c >> 6);
			(*output)++[0] = (char)(0x80 | (c & 0x3F));
			return 2;
		case c <= 0xffff:
			(*output)++[0] = (char)(0xE0 | c >> 12);
			(*output)++[0] = (char)(0x80 | (c >> 6 & 0x3F));
			(*output)++[0] = (char)(0x80 | (c & 0x3F));
			return 3;
		default:
			(*output)++[0] = (char)(0xF0 | c >> 18);
			(*output)++[0] = (char)(0x80 | (c >> 12 & 0x3F));
			(*output)++[0] = (char)(0x80 | (c >> 6 & 0x3F));
			(*output)++[0] = (char)(0x80 | (c & 0x3F));
			return 4;
	}
}

<*
 @param [in] ptr : `pointer to the first character to parse`
 @param [inout] size : `Set to max characters to read, set to characters read`
 @return `the parsed 32 bit codepoint`
*>
fn Char32? utf8_to_char32(char* ptr, sz* size)
{
	sz max_size = *size;
	if (max_size < 1) return string::INVALID_UTF8~;
	char c = (ptr++)[0];

	if ((c & 0x80) == 0)
	{
		*size = 1;
		return c;
	}
	if ((c & 0xE0) == 0xC0)
	{
		if (max_size < 2) return string::INVALID_UTF8~;
		*size = 2;
		Char32 uc = (c & 0x1F) << 6;
		c = *ptr;
		// Overlong sequence or invalid second.
		if (!uc || c & 0xC0 != 0x80) return string::INVALID_UTF8~;
		return uc + c & 0x3F;
	}
	if ((c & 0xF0) == 0xE0)
	{
		if (max_size < 3) return string::INVALID_UTF8~;
		*size = 3;
		Char32 uc = (c & 0x0F) << 12;
		c = ptr++[0];
		if (c & 0xC0 != 0x80) return string::INVALID_UTF8~;
		uc += (c & 0x3F) << 6;
		c = ptr++[0];
		// Overlong sequence or invalid last
		if (!uc || c & 0xC0 != 0x80) return string::INVALID_UTF8~;
		return uc + c & 0x3F;
	}
	if (max_size < 4) return string::INVALID_UTF8~;
	if ((c & 0xF8) != 0xF0) return string::INVALID_UTF8~;
	*size = 4;
	Char32 uc = (c & 0x07) << 18;
	c = ptr++[0];
	if (c & 0xC0 != 0x80) return string::INVALID_UTF8~;
	uc += (c & 0x3F) << 12;
	c = ptr++[0];
	if (c & 0xC0 != 0x80) return string::INVALID_UTF8~;
	uc += (c & 0x3F) << 6;
	c = ptr++[0];
	// Overlong sequence or invalid last
	if (!uc || c & 0xC0 != 0x80) return string::INVALID_UTF8~;
	return uc + c & 0x3F;
}

<*
 @param utf8 : `An UTF-8 encoded slice of bytes`
 @return `the number of encoded code points`
*>
fn sz utf8_codepoints(String utf8)
{
	sz len;
	foreach (char c : utf8)
	{
		if (c & 0xC0 != 0x80) len++;
	}
	return len;
}


<*
 Calculate the UTF8 length required to encode an UTF32 array.
 @param [in] utf32 : `the utf32 data to calculate from`
 @require utf32.len % 4 == 0 : "Data must be an even 4 bytes"
 @return `the length of the resulting UTF8 array`
*>
fn sz utf8len_for_utf32_bytes(char[] utf32, bool swap_endianness)
{
	Char32[] unaligned = ((Char32*)utf32.ptr)[:utf32.len / 4];
	if (@unlikely(swap_endianness))
	{
		sz len;
		foreach (&c : unaligned)
		{
			Char32 uc = mem::load(c, $align:1);
			len += utf8_len_for_char32(bswap(uc));
		}
		return len;
	}
	sz len;
	foreach (&c : unaligned)
	{
		len += utf8_len_for_char32(mem::load(c, $align:1));
	}
	return len;
}

<*
 Calculate the UTF8 length required to encode an UTF32 array.
 @param [in] utf32 : `the utf32 data to calculate from`
 @return `the length of the resulting UTF8 array`
*>
fn sz utf8len_for_utf32(Char32[] utf32)
{
	sz len;
	foreach (Char32 uc : utf32)
	{
		len += utf8_len_for_char32(uc);
	}
	return len;
}

macro sz utf8_len_for_char32(Char32 uc) @local
{
	switch
	{
		case uc <= 0x7f: return 1;
		case uc <= 0x7ff: return 2;
		case uc <= 0xffff: return 3;
		default: return 4;
	}
}

macro sz utf8_len_for_char16(Char16 c) @local
{
	switch
	{
		case c & UTF16_SURROGATE_GENERIC_MASK == UTF16_SURROGATE_GENERIC_VALUE: return 4;
		case c <= 0x7f: return 1;
		case c <= 0x7ff: return 2;
		default: return 3;
	}
}

<*
 Calculate the UTF8 length required to encode an UTF16 array.
 @param [in] utf16 : `the utf16 data to calculate from`
 @require utf16.len % 2 == 0 : "Data must be an even 2 bytes"
 @return `the length of the resulting UTF8 array`
*>
fn sz utf8len_for_utf16_bytes(char[] utf16, bool swap_endianness)
{
	Char16[] unaligned = ((Char16*)utf16.ptr)[:utf16.len / 2];
	if (@unlikely(swap_endianness))
	{
		sz len;
		foreach (&c : unaligned)
		{
			Char16 uc = mem::load(c, $align:1);
			len += utf8_len_for_char16(bswap(uc));
		}
		return len;
	}
	sz len;
	foreach (&c : unaligned)
	{
		len += utf8_len_for_char16(mem::load(c, $align:1));
	}
	return len;
}

<*
 Calculate the UTF8 length required to encode an UTF16 array.
 @param [in] utf16 : `the utf16 data to calculate from`
 @return `the length of the resulting UTF8 array`
*>
fn sz utf8len_for_utf16(Char16[] utf16)
{
	sz len;
	foreach (c : utf16)
	{
		len += utf8_len_for_char16(c);
	}
	return len;
}

<*
 Calculate the UTF16 length required to encode a UTF8 array.
 @param utf8 : `the utf8 data to calculate from`
 @return `the length of the resulting UTF16 array`
*>
fn sz utf16len_for_utf8(String utf8)
{
	sz len = utf8.len;
	sz len16;
	for (sz i = 0; i < len; i++)
	{
		len16++;
		char c = utf8[i];
		if (c & 0x80 == 0) continue;
		i++;
		if (c & 0xE0 == 0xC0) continue;
		i++;
		if (c & 0xF0 == 0xE0) continue;
		i++;
		len16++;
	}
	return len16;
}

<*
 @param [in] utf32 : `the UTF32 array to check the length for`
 @return `the required length of an UTF16 array to hold the UTF32 data.`
*>
fn sz utf16len_for_utf32(Char32[] utf32)
{
	sz len = utf32.len;
	foreach (Char32 uc : utf32)
	{
		if (uc >= UTF16_SURROGATE_OFFSET) len++;
	}
	return len;
}

<*
 Convert an UTF32 array to an UTF8 array.

 @param [in] utf32
 @param [out] utf8_buffer
 @return `the number of bytes written.`
*>
fn sz? utf32to8(Char32[] utf32, char[] utf8_buffer)
{
	char[] buffer = utf8_buffer;
	foreach (uc : utf32)
	{
		sz used = char32_to_utf8(uc, buffer) @inline!;
		buffer = buffer[used..];
	}
	// Zero terminate if there is space.
	if (buffer.len > 0) buffer[0] = 0;
	return utf8_buffer.len - buffer.len;
}

<*
 Convert an UTF32 array to an UTF8 array.

 @param [in] utf32
 @param [out] utf8_buffer
 @require utf32.len % 4 == 0 : "Length must be a multiple of 4"
 @return `the number of bytes written.`
*>
fn sz? utf32_bytes_to8(char[] utf32, char[] utf8_buffer, bool swap_endianess)
{
	char[] buffer = utf8_buffer;
	Char32[] unaligned = ((Char32*)utf32.ptr)[:utf32.len / 4];
	if (swap_endianess)
	{
		foreach (&uc : unaligned)
		{
			sz used = char32_to_utf8(bswap(mem::load(uc, $align:1)), buffer) @inline!;
			buffer = buffer[used..];
		}
	}
	else
	{
		if (((uptr)unaligned.ptr) % 4 == 0) return utf32to8(unaligned, utf8_buffer);
		foreach (&uc : unaligned)
		{
			sz used = char32_to_utf8(mem::load(uc, $align:1), buffer) @inline!;
			buffer = buffer[used..];
		}
	}
	// Zero terminate if there is space.
	if (buffer.len > 0) buffer[0] = 0;
	return utf8_buffer.len - buffer.len;
}

<*
 Convert an UTF8 array to an UTF32 array.

 @param [in] utf8
 @param [out] utf32_buffer
 @return `the number of Char32s written.`
*>
fn sz? utf8to32(String utf8, Char32[] utf32_buffer)
{
	sz len = utf8.len;
	Char32* ptr = utf32_buffer.ptr;
	sz len32;
	sz buf_len = utf32_buffer.len;
	for (sz i = 0; i < len;)
	{
		if (len32 == buf_len) return string::CONVERSION_FAILED~;
		sz width = len - i;
		Char32 uc = utf8_to_char32(&utf8[i], &width) @inline!;
		i += width;
		ptr[len32++] = uc;
	}
	// Zero terminate if possible
	if (len32 + 1 < buf_len) ptr[len32] = 0;
	return len32;
}

<*
 Copy an array of UTF16 data into an UTF8 buffer without bounds
 checking. This will assume the buffer is sufficiently large to hold
 the converted data.

 @param [in] utf16 : `The UTF16 array containing the data to convert.`
 @param [out] utf8_buffer : `the (sufficiently large) buffer to hold the UTF16 data.`
*>
macro void? utf16to8_unsafe(Char16[] utf16, char* utf8_buffer, $unaligned, $byteswap)
{
	sz len16 = utf16.len;
	for (sz i = 0; i < len16;)
	{
		sz available = len16 - i;
		char16_to_utf8_unsafe(&utf16[i], &available, &utf8_buffer, $unaligned, $byteswap) @inline!;
		i += available;
	}
}

<*
 Copy an array of UTF8 data into an UTF32 buffer without bounds
 checking. This will assume the buffer is sufficiently large to hold
 the converted data.

 @param [in] utf8 : `The UTF8 buffer containing the data to convert.`
 @param [out] utf32_buffer : `the (sufficiently large) buffer to hold the UTF8 data.`
*>
fn void? utf8to32_unsafe(String utf8, Char32* utf32_buffer)
{
	sz len = utf8.len;
	for (sz i = 0; i < len;)
	{
		sz width = len - i;
		Char32 uc = utf8_to_char32(&utf8[i], &width) @inline!;
		i += width;
		(utf32_buffer++)[0] = uc;
	}
}

<*
 Copy an array of UTF8 data into an UTF16 buffer without bounds
 checking. This will assume the buffer is sufficiently large to hold
 the converted data.

 @param [in] utf8 : `The UTF8 buffer containing the data to convert.`
 @param [out] utf16_buffer : `the (sufficiently large) buffer to hold the UTF8 data.`
*>
fn void? utf8to16_unsafe(String utf8, Char16* utf16_buffer)
{
	sz len = utf8.len;
	for (sz i = 0; i < len;)
	{
		sz width = len - i;
		Char32 uc = utf8_to_char32(&utf8[i], &width) @inline!;
		char32_to_utf16_unsafe(uc, &utf16_buffer) @inline;
		i += width;
	}
}

<*
 Copy an array of UTF32 code points into an UTF8 buffer without bounds
 checking. This will assume the buffer is sufficiently large to hold
 the converted data.

 @param [in] utf32 : `The UTF32 buffer containing the data to convert.`
 @param [out] utf8_buffer : `the (sufficiently large) buffer to hold the UTF8 data.`
 @param $unaligned : `If data should be loaded unaligned`
 @param $byteswap : `If the result should be byteswapped`
*>
macro void utf32to8_unsafe(Char32[] utf32, char* utf8_buffer, $unaligned, $byteswap)
{
	char* start = utf8_buffer;
	foreach (&ucref : utf32)
	{
		Char32 uc = $unaligned ??? mem::load(ucref, $align: 1) : *ucref;
		$if $byteswap:
			uc = bswap(uc);
		$endif
		char32_to_utf8_unsafe(uc, &utf8_buffer) @inline;
	}
}
