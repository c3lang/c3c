module std::hash::murmur3_test @test;
import std::hash::murmur3;

fn void hash32()
{
	test::eq(0, murmur3::hash32("", 0));
	test::eq(0x514E28B7, murmur3::hash32("", 1));
	test::eq(0x81F16F39, murmur3::hash32("", 0xffffffff));
	test::eq(0x2362F9DE, murmur3::hash32("\0\0\0\0", 0));
	test::eq(0x5A97808A, murmur3::hash32("aaaa", 0x9747b28c));
	test::eq(0x283E0130, murmur3::hash32("aaa", 0x9747b28c));
	test::eq(0x5D211726, murmur3::hash32("aa", 0x9747b28c));
	test::eq(0x7FA09EA6, murmur3::hash32("a", 0x9747b28c));
	test::eq(0xF0478627, murmur3::hash32("abcd", 0x9747b28c));
	test::eq(0xC84A62DD, murmur3::hash32("abc", 0x9747b28c));
	test::eq(0x74875592, murmur3::hash32("ab", 0x9747b28c));
	test::eq(0x7FA09EA6, murmur3::hash32("a", 0x9747b28c));
	test::eq(0x24884CBA, murmur3::hash32("Hello, world!", 0x9747b28c));
	test::eq(0xD58063C1, murmur3::hash32("ππππππππ", 0x9747b28c));
	char[256] test = { [0..255] = 'a' };
	test::eq(0x37405BDC, murmur3::hash32(&test, 0x9747b28c));
}

fn void hash128_64()
{
	test::eq(0, murmur3::hash128_64("", 0));
	test::eq(0x51622daa78f835834610abe56eff5cb5, murmur3::hash128_64("", 1));
	test::eq(0x857421121ee6446b6af1df4d9d3bc9ec, murmur3::hash128_64("", 0xffffffff));
	test::eq(0x589623161cf526f1cfa0f7ddd84c76bc, murmur3::hash128_64("\0\0\0\0", 0));
	test::eq(0xf66e73e07751664edbcf7463becf7e04, murmur3::hash128_64("xxxxxxxxxxxxxxxxxxxxxxxxxxxx", 123));
	test::eq(0xf19732fdd373c3f5421c8c738743acad, murmur3::hash128_64("Hello, world!", 123));
	test::eq(0x79200aeeb9546c79ca47f42bf86d4004, murmur3::hash128_64("Hello, world!", 321));
}

fn void hash128_32()
{
	test::eq(0, murmur3::hash128_32("", 0));
	test::eq(0x26f3e79926f3e79926f3e799fedc5245, murmur3::hash128_32("", 123));
	test::eq(0x989d49f7989d49f7989d49f7051e08a9, murmur3::hash128_32("", 0xFFFFFFFF));
	test::eq(0x9e5178409e5178409e517840cc066f1f, murmur3::hash128_32("\0\0\0\0", 0));
	test::eq(0x1fec60474cf929d378825a165e40bab2, murmur3::hash128_32("xxxxxxxxxxxxxxxxxxxxxxxxxxxx", 123));
	test::eq(0x9e37c886a41621625a1aacd761c9129e, murmur3::hash128_32("Hello, world!", 123));
	test::eq(0xa7170f0f045880c5c26c4193d5fbdcb3, murmur3::hash128_32("Hello, world!", 321));
}