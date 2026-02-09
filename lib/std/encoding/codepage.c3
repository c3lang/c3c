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

<* Returns a CodePageTable for the given charset name. *>
fn CodePageTable*? charset(String charset_name) => @pool()
{
	String name = charset_name.treplace("_","-");
	name.convert_to_lower();
	switch (name)
	{
		case "cp1250": return &CP1250;
		case "cp1251": return &CP1251;
		case "cp1252": return &CP1252;
		case "cp1253": return &CP1253;
		case "cp1254": return &CP1254;
		case "cp1255": return &CP1255;
		case "cp1256": return &CP1256;
		case "cp1257": return &CP1257;
		case "cp1258": return &CP1258;
		case "cp437": return &CP437;
		case "cp737": return &CP737;
		case "cp775": return &CP775;
		case "cp850": return &CP850;
		case "cp852": return &CP852;
		case "cp855": return &CP855;
		case "cp857": return &CP857;
		case "cp860": return &CP860;
		case "cp861": return &CP861;
		case "cp862": return &CP862;
		case "cp863": return &CP863;
		case "cp864": return &CP864;
		case "cp865": return &CP865;
		case "cp866": return &CP866;
		case "cp869": return &CP869;
		case "cp874": return &CP874;
		case "iso-8859-1": return &ISO_8859_1;
		case "iso-8859-10": return &ISO_8859_10;
		case "iso-8859-11": return &ISO_8859_11;
		case "iso-8859-13": return &ISO_8859_13;
		case "iso-8859-14": return &ISO_8859_14;
		case "iso-8859-15": return &ISO_8859_15;
		case "iso-8859-16": return &ISO_8859_16;
		case "iso-8859-2": return &ISO_8859_2;
		case "iso-8859-3": return &ISO_8859_3;
		case "iso-8859-4": return &ISO_8859_4;
		case "iso-8859-5": return &ISO_8859_5;
		case "iso-8859-6": return &ISO_8859_6;
		case "iso-8859-7": return &ISO_8859_7;
		case "iso-8859-8": return &ISO_8859_8;
		case "iso-8859-9": return &ISO_8859_9;
		case "us-ascii": return &US_ASCII;
		default: return NOT_FOUND~;
	}
}


fn String? decode(Allocator allocator, char[] src, CodePageTable *table)
{
	char[] dst = allocator::alloc_array(allocator, char, decode_len(src, table));
	return decode_buffer(src, dst, table);
}

<*
 Decode a code-page byte buffer into a UTF‑8 string.

 @param src            : "Input byte array in the given code page."
 @param dst            : "Destination output string in UTF-8."
 @param [&inout] table : "Code page table describing the mapping for this encoding."
 @return "String in UTF-8."
*>
fn String? decode_buffer(char[] src, char[] dst, CodePageTable *table)
{
	usz n = 0;
	foreach (c: src)
	{
		usz pos = (usz)c * 4;
		char len = table.to_codepoint[pos];

		dst[n:len] = table.to_codepoint[pos+1:len];
		n += len;
	}

	return (String)dst[:n];
}

fn char[]? encode(Allocator allocator, char[] src, CodePageTable *table,
	char replacement = REPLACEMENT_CHAR)
{
	char[] dst = allocator::alloc_array(allocator, char, encode_len(src));
	return encode_buffer(src, dst, table, replacement);
}

const uint MASK @private = (1u << 24) - 1;

<*
 Encode a UTF‑8 string into a single‑byte code page.

 @param src            : "Input byte array in UTF-8"
 @param dst            : "Destination output byte array in the target code page"
 @param [&inout] table : "Code page table describing the mapping for this encoding."
 @param replacement    : "Byte to emit when Unicode scalar cannot be represented in the target code page."
 @return "Byte array in the given code page."
*>
fn char[]? encode_buffer(char[] src, char[] dst, CodePageTable *table,
	char replacement = REPLACEMENT_CHAR)
{
	// Unpack the packed reverse table once into a local uint[256] view.
	uint[256] from_map;
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
		usz index = sort::binarysearch(from_map[..], (uint)codepoint,
		    fn int(uint lhs, uint rhs) => (int)(lhs & MASK) - (int)(rhs & MASK));

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
 @param [&inout] table     : "Code page table describing the mapping for this encoding."
*>
fn usz decode_len(char[] src, CodePageTable *table) @inline
{
	usz n;
	foreach (usz c: src) n += table.to_codepoint[c*4];
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

