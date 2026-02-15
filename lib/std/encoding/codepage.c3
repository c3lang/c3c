// Copyright (c) 2026 Koni Marti. All rights reserved.
// Use of this source code is governed by the MIT license.
<*
 Module providing generic single‑byte code page to UTF‑8 conversion.

 This module implements a compact, table‑driven approach for single-byte
 (8‑bit) encodings (e.g. CP437, CP850, CP866, CP125x). It is designed so
 that each concrete code page only needs to supply a small, static
 mapping table; the conversion logic is shared.

 The design has two main goals:

 - Fast decode from code page to UTF‑8 with a single table lookup per byte.
 - Memory‑efficient encode from UTF‑8 to code page without a large
   Unicode‑to‑byte array (no 64k reverse map per code page).

 The design of CodePageTable and the packed reverse mapping is conceptually
 similar to golang.org/x/text/encoding/charmap.

 *>
module std::encoding::codepage;
import std::sort;

<*
 Default replacement byte used when encoding from UTF‑8 to a single‑byte
 code page and a Unicode scalar cannot be represented.

 By convention, 0x1A is the ASCII/IBM SUB (substitute) control character.
*>
const char REPLACEMENT_CHAR = 0x1a;

<*
 CodePageTable contains the bidirectional mapping tables for a single‑byte code
 page in a compact packed form.

 to_codepoint is the forward map from code‑page byte (0x00–0xFF) to its UTF‑8
 sequence. The array index is the raw byte value, each entry occupying 4 bytes:

   - Byte 0 is the length of the UTF‑8 sequence (0–4)
   - Bytes 1:len are the UTF‑8 bytes for the mapped Unicode scalar

 The table therefore uses 256 * 4 bytes and is stored as a flat
 char[1024] array, where entry i starts at offset i*4.

 from_codepoint is the reverse map from Unicode scalar value to code‑page byte,
 also stored as a packed char[1024] array. It contains 256 entries of 4 bytes
 each, where each 4‑byte chunk is interpreted as a little‑endian uint
 with the following packing scheme:

     high 8 bits = code‑page byte value (0x00–0xFF)
     low 24 bits = Unicode scalar value (code point)

 In other words:

     entry = (byte_value << 24) | codepoint;

 Ordering:
   The 256 packed uint entries in from_codepoint are sorted by the low 24 bits
   (code points). This allows binary search over Unicode scalar values without
   a 64k reverse‑lookup table. For any given code page, there are at most
   256 mappings, so a log2(256) or 8 step search is sufficient.
*>
struct CodePageTable
{
    char[1024] to_codepoint;
    char[1024] from_codepoint;
}

enum CodePage : (String name, CodePageTable* table)
{
	CP1250      { "cp1250",      &codepage::CP1250      },
	CP1251      { "cp1251",      &codepage::CP1251      },
	CP1252      { "cp1252",      &codepage::CP1252      },
	CP1253      { "cp1253",      &codepage::CP1253      },
	CP1254      { "cp1254",      &codepage::CP1254      },
	CP1255      { "cp1255",      &codepage::CP1255      },
	CP1256      { "cp1256",      &codepage::CP1256      },
	CP1257      { "cp1257",      &codepage::CP1257      },
	CP1258      { "cp1258",      &codepage::CP1258      },
	CP437       { "cp437",       &codepage::CP437       },
	CP737       { "cp737",       &codepage::CP737       },
	CP775       { "cp775",       &codepage::CP775       },
	CP850       { "cp850",       &codepage::CP850       },
	CP852       { "cp852",       &codepage::CP852       },
	CP855       { "cp855",       &codepage::CP855       },
	CP857       { "cp857",       &codepage::CP857       },
	CP860       { "cp860",       &codepage::CP860       },
	CP861       { "cp861",       &codepage::CP861       },
	CP862       { "cp862",       &codepage::CP862       },
	CP863       { "cp863",       &codepage::CP863       },
	CP864       { "cp864",       &codepage::CP864       },
	CP865       { "cp865",       &codepage::CP865       },
	CP866       { "cp866",       &codepage::CP866       },
	CP869       { "cp869",       &codepage::CP869       },
	CP874       { "cp874",       &codepage::CP874       },
	ISO_8859_1  { "iso-8859-1",  &codepage::ISO_8859_1  },
	ISO_8859_10 { "iso-8859-10", &codepage::ISO_8859_10 },
	ISO_8859_11 { "iso-8859-11", &codepage::ISO_8859_11 },
	ISO_8859_13 { "iso-8859-13", &codepage::ISO_8859_13 },
	ISO_8859_14 { "iso-8859-14", &codepage::ISO_8859_14 },
	ISO_8859_15 { "iso-8859-15", &codepage::ISO_8859_15 },
	ISO_8859_16 { "iso-8859-16", &codepage::ISO_8859_16 },
	ISO_8859_2  { "iso-8859-2",  &codepage::ISO_8859_2  },
	ISO_8859_3  { "iso-8859-3",  &codepage::ISO_8859_3  },
	ISO_8859_4  { "iso-8859-4",  &codepage::ISO_8859_4  },
	ISO_8859_5  { "iso-8859-5",  &codepage::ISO_8859_5  },
	ISO_8859_6  { "iso-8859-6",  &codepage::ISO_8859_6  },
	ISO_8859_7  { "iso-8859-7",  &codepage::ISO_8859_7  },
	ISO_8859_8  { "iso-8859-8",  &codepage::ISO_8859_8  },
	ISO_8859_9  { "iso-8859-9",  &codepage::ISO_8859_9  },
	US_ASCII    { "us-ascii",    &codepage::US_ASCII    },
}

<*
 Returns a CodePage for the given charset name.

 @param [in] charset_name   :  "A name, case insensitive, using _ or - for separator"
 @return "The CodePage for the name"

 @return? NOT_FOUND : "If the charset is unknown or unsupported"
*>
fn CodePage? by_name(String charset_name) => @pool()
{
	String name = charset_name.treplace("_","-");
	name.convert_to_lower();
	foreach (page : CodePage.values)
	{
		if (page.name == charset_name) return page;
	}
	return NOT_FOUND~;
}

fn String? decode(Allocator allocator, char[] src, CodePage code_page)
{
	char[] dst = allocator::alloc_array(allocator, char, decode_len(src, code_page));
	return decode_buffer(src, dst, code_page);
}

<*
 Decode a code-page byte buffer into a UTF‑8 string.

 @param src            : "Input byte array in the given code page."
 @param dst            : "Destination output string in UTF-8."
 @param code_page      : "Code page for this encoding."
 @return "String in UTF-8."
*>
fn String? decode_buffer(char[] src, char[] dst, CodePage code_page)
{
	usz n = 0;
	CodePageTable *table = code_page.table;
	foreach (c: src)
	{
		usz pos = (usz)c * 4;
		char len = table.to_codepoint[pos];

		dst[n:len] = table.to_codepoint[pos+1:len];
		n += len;
	}

	return (String)dst[:n];
}

fn char[]? encode(Allocator allocator, char[] src, CodePage code_page, char replacement = REPLACEMENT_CHAR)
{
	char[] dst = allocator::alloc_array(allocator, char, encode_len(src));
	return encode_buffer(src, dst, code_page, replacement);
}

const uint MASK @private = (1u << 24) - 1;

<*
 Encode a UTF‑8 string into a single‑byte code page.

 @param src            : "Input byte array in UTF-8"
 @param dst            : "Destination output byte array in the target code page"
 @param code_page      : "Code page for this encoding."
 @param replacement    : "Byte to emit when Unicode scalar cannot be represented in the target code page."
 @return "Byte array in the given code page."
*>
fn char[]? encode_buffer(char[] src, char[] dst, CodePage code_page, char replacement = REPLACEMENT_CHAR)
{
	// Unpack the packed reverse table once into a local uint[256] view.
	uint[256] from_map;
	CodePageTable *table = code_page.table;
	for (usz i = 0; i < 256; i++)
	{
	    UIntLE *val = (UIntLE*)&table.from_codepoint[i * 4];
	    from_map[i] = mem::load(val, 1).val;
	}

	usz out = 0;
	usz n = src.len;
	for (usz i = 0; i < n; )
	{
		usz rem = n - i;
		if (rem > 4) rem = 4;

		Char32 codepoint = conv::utf8_to_char32(&src[i], &rem)!;
		i += rem;

		// Binary search for codepoint in low 24 bits of each entry.
		// Returned index is between [0..from_map.len).
		usz index = sort::binarysearch(from_map[..], (uint)codepoint, fn int(uint lhs, uint rhs) => (int)(lhs & MASK) - (int)(rhs & MASK));

		uint entry = from_map[index];
		if ((entry & MASK) == (uint)codepoint)
		{
		    char b = (char)(entry >> 24);
		    dst[out++] = b;
		}
		else
		{
		    dst[out++] = replacement;
		}
	}

	return dst[:out];
}

<*
 Compute the number of UTF‑8 bytes produced when decoding src with the given
 code page table.
 @param src                : "Input byte array in the given code page."
 @param code_page          : "Code page for this encoding."
*>
fn usz decode_len(char[] src, CodePage code_page) @inline
{
	usz n;
	CodePageTable *table = code_page.table;
	foreach (usz c: src) n += table.to_codepoint[c *4 ];
	return n;
}

<*
 Compute the number of output bytes produced when
 encoding src from UTF‑8 to a single‑byte code page.
 @param src                : "Input byte array in UTF-8"
*>
fn usz encode_len(char[] src) @inline
{
	return conv::utf8_codepoints((String)src);
}

