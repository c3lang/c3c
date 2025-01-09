module conv_tests;
import std::io;

fn void! comparison_helper_32_to_8(Char32 c32, String expected_output)
{
	char[8] out;
	usz len = conv::char32_to_utf8(c32, &out)!;
	assert(len == expected_output.len, "Len should be 1");
	foreach (i, c : expected_output)
	{
		assert(out[i] == c, "Expected other value");
	}
}

fn void! comparison_helper_8_to_32(String in, Char32 c32)
{
	usz len = in.len;
	Char32 res = conv::utf8_to_char32(in.ptr, &len)!;
	assert(len == in.len, "All len should be used.");
	assert(res == c32, "Expected character match.");
}

fn void assert_utf8_is_error(String in)
{
	usz len = in.len;
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
	assert_utf8_is_error({ 0xe0, 0x80, 0xaf });
	assert_utf8_is_error({ 0xf0, 0x80, 0x80, 0xaf });
	assert_utf8_is_error({ 0xf8, 0x80, 0x80, 0xaf });
	assert_utf8_is_error({ 0xfc, 0x80, 0x80, 0x80, 0xaf });
}

