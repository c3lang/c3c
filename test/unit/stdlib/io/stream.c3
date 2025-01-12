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
	bw.temp_init();
	io::write_be_short(&bw, 0x348a)!!;
	assert(bw.str_view() == &&x'348a');
}

fn void write_uint_test()
{
	ByteWriter bw;
	bw.temp_init();
	io::write_be_int(&bw, 0x3421348a)!!;
	assert(bw.str_view() == &&x'3421348a');
}

fn void write_ulong_test()
{
	ByteWriter bw;
	bw.temp_init();
	io::write_be_long(&bw, 0xaabbccdd3421348a)!!;
	assert(bw.str_view() == &&x'aabbccdd3421348a');
}

fn void write_uint128_test()
{
	ByteWriter bw;
	bw.temp_init();
	io::write_be_int128(&bw, 0xaabbccdd3421348aaabbccdd3421348a)!!;
	assert(bw.str_view() == &&x'aabbccdd3421348aaabbccdd3421348a');
}

fn void write_tiny_bytearray_test()
{
	ByteWriter bw;
	bw.temp_init();
	io::write_tiny_bytearray(&bw, &&x"aabbcc00112233")!!;
	assert(bw.str_view() == &&x'07aabbcc00112233');
}

fn void write_short_bytearray_test()
{
	ByteWriter bw;
	bw.temp_init();
	io::write_short_bytearray(&bw, &&x"aabbcc00112233")!!;
	assert(bw.str_view() == &&x'0007aabbcc00112233');
}

fn void read_tiny_bytearray_test()
{
	ByteReader reader = io::wrap_bytes(&&x'07aabbcc00112233');
	char[] read = io::read_tiny_bytearray(&reader, allocator: allocator::heap())!!;
	assert(read == &&x'aabbcc00112233');
}

fn void read_short_bytearray_test()
{
	ByteReader reader = io::wrap_bytes(&&x'0007aabbcc00112233');
	char[] read = io::read_short_bytearray(&reader, allocator: allocator::heap())!!;
	assert(read == &&x'aabbcc00112233');
}
