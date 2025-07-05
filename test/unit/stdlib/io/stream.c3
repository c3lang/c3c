module std::io @test;

fn void read_ushort_test()
{
	ByteReader reader = io::wrap_bytes({0x34, 0x8a});
	assert(io::read_be_ushort(&reader)!! == 0x348a);
}

fn void read_uint_test()
{
	ByteReader reader = io::wrap_bytes({0x34, 0x8a, 0xef, 0xcc});
	assert(io::read_be_uint(&reader)!! == 0x348aefcc);
}

fn void read_ulong_test()
{
	ByteReader reader = io::wrap_bytes({0x34, 0x8a, 0xef, 0xcc, 0x34, 0x8a, 0xef, 0xcc});
	assert(io::read_be_ulong(&reader)!! == 0x348aefcc348aefcc);
}

fn void read_uint128_test()
{
	ByteReader reader = io::wrap_bytes({0x34, 0x8a, 0xef, 0xcc, 0x34, 0x8a, 0xef, 0xcc, 0x34, 0x8a, 0xef, 0xcc, 0x34, 0x8a, 0xef, 0xcc});
	assert(io::read_be_uint128(&reader)!! == 0x348aefcc348aefcc348aefcc348aefcc);
}

fn void write_ushort_test()
{
	ByteWriter bw;
	bw.tinit();
	io::write_be_short(&bw, 0x348a)!!;
	assert(bw.str_view() == &&x'348a');
}

fn void write_uint_test()
{
	ByteWriter bw;
	bw.tinit();
	io::write_be_int(&bw, 0x3421348a)!!;
	assert(bw.str_view() == &&x'3421348a');
}

fn void write_ulong_test()
{
	ByteWriter bw;
	bw.tinit();
	io::write_be_long(&bw, 0xaabbccdd3421348a)!!;
	assert(bw.str_view() == &&x'aabbccdd3421348a');
}

fn void write_uint128_test()
{
	ByteWriter bw;
	bw.tinit();
	io::write_be_int128(&bw, 0xaabbccdd3421348aaabbccdd3421348a)!!;
	assert(bw.str_view() == &&x'aabbccdd3421348aaabbccdd3421348a');
}

fn void write_tiny_bytearray_test()
{
	ByteWriter bw;
	bw.tinit();
	io::write_tiny_bytearray(&bw, &&x"aabbcc00112233")!!;
	assert(bw.str_view() == &&x'07aabbcc00112233');
}

fn void write_short_bytearray_test()
{
	ByteWriter bw;
	bw.tinit();
	io::write_short_bytearray(&bw, &&x"aabbcc00112233")!!;
	assert(bw.str_view() == &&x'0007aabbcc00112233');
}

fn void read_tiny_bytearray_test()
{
	ByteReader reader = io::wrap_bytes(&&x'07aabbcc00112233');
	char[] read = io::read_tiny_bytearray(&reader, allocator: allocator::heap())!!;
	assert(read == &&x'aabbcc00112233');
	free(read);
}

fn void read_short_bytearray_test()
{
	ByteReader reader = io::wrap_bytes(&&x'0007aabbcc00112233');
	char[] read = io::read_short_bytearray(&reader, allocator: allocator::heap())!!;
	assert(read == &&x'aabbcc00112233');
	free(read);
}

fn void read_until_delimiter_test()
{
	// Test basic functionality - read until delimiter found
	ByteReader reader = io::wrap_bytes("hello,world");
	ByteWriter writer;
	writer.tinit();

	usz bytes_read = io::read_until_delimiter(&reader, &writer, ",")!!;
	assert(bytes_read == 5);
	assert(writer.str_view() == "hello");
}

fn void read_until_delimiter_eof_test()
{
	// Test EOF case - delimiter not found
	ByteReader reader = io::wrap_bytes("hello");
	ByteWriter writer;
	writer.tinit();

	assert(@catch(io::read_until_delimiter(&reader, &writer, ",")));
}

fn void read_until_delimiter_empty_test()
{
	// Test empty input
	ByteReader reader = io::wrap_bytes("");
	ByteWriter writer;
	writer.tinit();

	assert(@catch(io::read_until_delimiter(&reader, &writer, ",")));
}

fn void read_until_delimiter_immediate_test()
{
	// Test delimiter at start
	ByteReader reader = io::wrap_bytes(",hello");
	ByteWriter writer;
	writer.tinit();

	usz bytes_read = io::read_until_delimiter(&reader, &writer, ",")!!;
	assert(bytes_read == 0);
	assert(writer.str_view() == "");
}

fn void read_until_delimiter_output_full_test()
{
	// Test output stream full/failed to write
	ByteReader reader = io::wrap_bytes("hello,world");
	ByteWriter writer;
	char[3] small_buffer;
	writer.init_with_buffer(&small_buffer);

	// This should fail when trying to write the 4th byte since buffer is only 3 bytes
	assert(@catch(io::read_until_delimiter(&reader, &writer, ",")));
}

fn void read_until_delimiter_crlf_test()
{
	// Test CRLF delimiter
	ByteReader reader = io::wrap_bytes("hello\r\nworld");
	ByteWriter writer;
	writer.tinit();

	usz bytes_read = io::read_until_delimiter(&reader, &writer, "\r\n")!!;
	assert(bytes_read == 5);
	assert(writer.str_view() == "hello");
}

fn void read_until_delimiter_single_char_test()
{
	// Test single character - should behave like read_until_delimiter
	ByteReader reader = io::wrap_bytes("hello,world");
	ByteWriter writer;
	writer.tinit();

	usz bytes_read = io::read_until_delimiter(&reader, &writer, ",")!!;
	assert(bytes_read == 5);
	assert(writer.str_view() == "hello");
}

fn void read_until_delimiter_empty_delimiter_test()
{
	// Test empty delimiter - should return 0
	ByteReader reader = io::wrap_bytes("hello");
	ByteWriter writer;
	writer.tinit();

	usz bytes_read = io::read_until_delimiter(&reader, &writer, "")!!;
	assert(bytes_read == 0);
	assert(writer.str_view() == "");
}

fn void read_until_delimiter_partial_match_test()
{
	// Test partial match that doesn't complete
	ByteReader reader = io::wrap_bytes("hello\rworld\r\nend");
	ByteWriter writer;
	writer.tinit();

	usz bytes_read = io::read_until_delimiter(&reader, &writer, "\r\n")!!;
	assert(bytes_read == 11); // "hello\rworld"
	assert(writer.str_view() == "hello\rworld");
}

fn void read_until_delimiter_multichar_eof_test()
{
	// Test EOF without finding delimiter
	ByteReader reader = io::wrap_bytes("hello\rworld");
	ByteWriter writer;
	writer.tinit();

	assert(@catch(io::read_until_delimiter(&reader, &writer, "\r\n")));
}
