module conv_tests;
import std::io;

fn void? comparison_helper_32_to_8(Char32 c32, String expected_output)
{
	char[8] out;
	sz len = conv::char32_to_utf8(c32, &out)!;
	assert(len == expected_output.len, "Len should be 1");
	foreach (i, c : expected_output)
	{
		assert(out[i] == c, "Expected other value");
	}
}

fn void? comparison_helper_8_to_32(String in, Char32 c32)
{
	sz len = in.len;
	Char32 res = conv::utf8_to_char32(in.ptr, &len)!;
	assert(len == in.len, "All len should be used.");
	assert(res == c32, "Expected character match.");
}

fn void assert_utf8_is_error(String in)
{
	sz len = in.len;
	assert(@catch(conv::utf8_to_char32(in.ptr, &len)), "Expected error");
}

fn void test_char32_ut8_boundary() @test
{
	// First sequence per len
	comparison_helper_32_to_8(0x00000000, { 0 })!!;
	comparison_helper_32_to_8(0x00000080, { 0xc2, 0x80 })!!;
	comparison_helper_32_to_8(0x00000800, { 0xe0, 0xa0, 0x80 })!!;
	comparison_helper_32_to_8(0x00010000, { 0xf0, 0x90, 0x80, 0x80 })!!;
	assert(@catch(comparison_helper_32_to_8(0x10ffff + 1, { 0 })), "Expected error");
	// Last seq per len
	comparison_helper_32_to_8(0x0000007f, { 0x7f })!!;
	comparison_helper_32_to_8(0x000007ff, { 0xdf, 0xbf })!!;
	comparison_helper_32_to_8(0x0000ffff, { 0xef, 0xbf, 0xbf })!!;
	comparison_helper_32_to_8(0x0010ffff, { 0xf4, 0x8f, 0xbf, 0xbf })!!;
	// Other boundaries
	comparison_helper_32_to_8(0x0000d7ff, { 0xed, 0x9f, 0xbf})!!;
	comparison_helper_32_to_8(0x0000e000, { 0xee, 0x80, 0x80 })!!;
	comparison_helper_32_to_8(0x0000fffd, { 0xef, 0xbf, 0xbd })!!;

}

fn void test_utf8_to_char32_boundary() @test
{
	// First sequence per len
	comparison_helper_8_to_32("\0", 0x0 )!!;
	comparison_helper_8_to_32({ 0xc2, 0x80 }, 0x80)!!;
	comparison_helper_8_to_32({ 0xe0, 0xa0, 0x80 }, 0x800 )!!;
	comparison_helper_8_to_32({ 0xf0, 0x90, 0x80, 0x80 }, 0x10000)!!;
	// Last seq per len
	comparison_helper_8_to_32({ 0x7f }, 0x7f)!!;
	comparison_helper_8_to_32({ 0xdf, 0xbf }, 0x7ff )!!;
	comparison_helper_8_to_32({ 0xef, 0xbf, 0xbf }, 0xffff)!!;
	comparison_helper_8_to_32({ 0xf4, 0x8f, 0xbf, 0xbf }, 0x10ffff)!!;
	// Other boundaries
	comparison_helper_8_to_32({ 0xed, 0x9f, 0xbf }, 0xd7ff)!!;
	comparison_helper_8_to_32({ 0xee, 0x80, 0x80 }, 0xe000)!!;
	comparison_helper_8_to_32({ 0xef, 0xbf, 0xbd }, 0xfffd)!!;

	assert_utf8_is_error({ 0x80 });
	assert_utf8_is_error({ 0xbf });
	assert_utf8_is_error({ 0xfe });
	assert_utf8_is_error({ 0xff });
	assert_utf8_is_error({ 0xfe, 0xfe, 0xff, 0xff });

	// Overlong
	assert_utf8_is_error({ 0xc0, 0xaf });
	assert_utf8_is_error({ 0xc1, 0xbf });
	assert_utf8_is_error({ 0xe0, 0x80, 0xaf });
	assert_utf8_is_error({ 0xe0, 0x9f, 0xbf });
	assert_utf8_is_error({ 0xf0, 0x80, 0x80, 0xaf });
	assert_utf8_is_error({ 0xf0, 0x8f, 0xbf, 0xbf });
	assert_utf8_is_error({ 0xf8, 0x80, 0x80, 0xaf });
	assert_utf8_is_error({ 0xfc, 0x80, 0x80, 0x80, 0xaf });

	// Surrogates and codepoints above U+10FFFF
	assert_utf8_is_error({ 0xed, 0xa0, 0x80 });
	assert_utf8_is_error({ 0xf4, 0x90, 0x80, 0x80 });
	assert_utf8_is_error({ 0xf5, 0x80, 0x80, 0x80 });
}

fn void test_utf16_surrogate_pair_roundtrip() @test
{
	// U+1F44D (THUMBS UP SIGN) requires a UTF-16 surrogate pair and a 4-byte UTF-8 sequence.
	Char32 c = 0x1F44D;

	Char16[4] utf16_buf;
	Char16* p = &utf16_buf;
	conv::char32_to_utf16_unsafe(c, &p);
	assert(utf16_buf[0] == (Char16)0xD83D, "Expected high surrogate 0xD83D");
	assert(utf16_buf[1] == (Char16)0xDC4D, "Expected low surrogate 0xDC4D");

	char[8] utf8_buf;
	conv::utf16to8_unsafe(utf16_buf[:2], &utf8_buf, false, false)!!;
	assert((String)utf8_buf[:4] == (String){ 0xf0, 0x9f, 0x91, 0x8d }, "Expected 4-byte UTF-8 sequence");

	Char16[4] roundtrip;
	conv::utf8to16_unsafe((String)utf8_buf[:4], &roundtrip)!!;
	assert(roundtrip[0] == utf16_buf[0] && roundtrip[1] == utf16_buf[1], "Expected surrogate pair to round-trip");

	assert(conv::utf16len_for_utf8((String)utf8_buf[:4]) == 2, "Expected 2 UTF-16 units for the surrogate pair");
	assert(conv::utf8len_for_utf16(utf16_buf[:2]) == 4, "Expected 4 UTF-8 bytes for the surrogate pair");
	assert(conv::utf16len_for_utf32((Char32[]){ c }) == 2, "Expected the astral codepoint to need 2 UTF-16 units");
}

fn void test_utf16_mixed_bmp_and_astral_roundtrip() @test
{
	// 'A', U+1F44D (surrogate pair), 'B' - mixes single-unit and surrogate-pair codepoints.
	String s = "A\U0001F44DB";
	assert(s.len == 6, "Expected 6 UTF-8 bytes");

	assert(conv::utf16len_for_utf8(s) == 4, "Expected 4 UTF-16 units (1 + 2 + 1)");

	Char16[8] utf16_buf;
	conv::utf8to16_unsafe(s, &utf16_buf)!!;
	assert(utf16_buf[0] == (Char16)'A');
	assert(utf16_buf[1] == (Char16)0xD83D);
	assert(utf16_buf[2] == (Char16)0xDC4D);
	assert(utf16_buf[3] == (Char16)'B');

	assert(conv::utf8len_for_utf16(utf16_buf[:4]) == 6, "Expected 6 UTF-8 bytes back");

	char[8] utf8_buf;
	conv::utf16to8_unsafe(utf16_buf[:4], &utf8_buf, false, false)!!;
	assert((String)utf8_buf[:6] == s, "Expected the mixed string to round-trip through UTF-16");
}

fn void test_utf16_invalid_surrogates() @test
{
	char[8] out;

	// Unmatched high surrogate at the end of the input.
	Char16[1] lone_high = { 0xD83D };
	assert(@catch(conv::utf16to8_unsafe(lone_high[:1], &out, false, false)) == string::INVALID_UTF16);

	// High surrogate not followed by a low surrogate.
	Char16[2] bad_pair = { 0xD83D, (Char16)'A' };
	assert(@catch(conv::utf16to8_unsafe(bad_pair[:2], &out, false, false)) == string::INVALID_UTF16);

	// Low surrogate appearing first, with no preceding high surrogate.
	Char16[1] lone_low = { 0xDC4D };
	assert(@catch(conv::utf16to8_unsafe(lone_low[:1], &out, false, false)) == string::INVALID_UTF16);
}
