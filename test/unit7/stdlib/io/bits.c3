module std::io::bits @test;
import std::io;

fn void test_write_0b1() {
	ByteWriter w;
	w.temp_init();

	BitWriter bw;
	bw.init(&w);

	bw.write_bits(0b11111111, 1)!!;
	bw.flush()!!;

	assert(w.str_view() == x"80"); // 0b1000 0000
}

fn void test_write_0b1111() {
	ByteWriter w;
	w.temp_init();

	BitWriter bw;
	bw.init(&w);

	bw.write_bits(0b11111111, 4)!!;
	bw.flush()!!;

	assert(w.str_view() == x"f0"); // 0b1111 0000
}

fn void test_write_0b1111_1111() {
	ByteWriter w;
	w.temp_init();

	BitWriter bw;
	bw.init(&w);

	bw.write_bits(0b11111111, 8)!!;
	bw.flush()!!;

	assert(w.str_view() == x"ff");
}

fn void test_write_0b1000() {
	ByteWriter w;
	w.temp_init();

	BitWriter bw;
	bw.init(&w);

	bw.write_bits(0b0001000, 4)!!;
	bw.flush()!!;

	assert(w.str_view() == x"80"); // 0b1000 0000
}

fn void test_write_0b01000() {
	ByteWriter w;
	w.temp_init();

	BitWriter bw;
	bw.init(&w);

	bw.write_bits(0b0001000, 5)!!;
	bw.flush()!!;

	assert(w.str_view() == x"40"); // 0b0100 0000
}

fn void test_write_0b0001() {
	ByteWriter w;
	w.temp_init();

	BitWriter bw;
	bw.init(&w);

	bw.write_bits(0b0000001, 4)!!;
	bw.flush()!!;

	assert(w.str_view() == x"10"); // 0b0001 0000
}


fn void test_write_0b0000_0001() {
	ByteWriter w;
	w.temp_init();

	BitWriter bw;
	bw.init(&w);

	bw.write_bits(0b0000001, 8)!!;
	bw.flush()!!;

	assert(w.str_view() == x"01"); // 0b0000 0001
}

fn void test_write_10_bits() {
	ByteWriter w;
	w.temp_init();

	BitWriter bw;
	bw.init(&w);

	bw.write_bits(0x789, 10)!!; // 01|11 1000 1001
	bw.flush()!!;

   								      //      e    2    4    0
	assert(w.str_view() == x"e2 40"); // 0b1110 0010 0100 0000
}

fn void test_write_16_bits() {
	ByteWriter w;
	w.temp_init();

	BitWriter bw;
	bw.init(&w);

	bw.write_bits(0xfafb, 16)!!;
	bw.flush()!!;

	assert(w.str_view() == x"fa fb");
}

fn void test_write_24_bits() {
	ByteWriter w;
	w.temp_init();

	BitWriter bw;
	bw.init(&w);

	bw.write_bits(0xfafbfc, 24)!!;
	bw.flush()!!;

	assert(w.str_view() == x"fa fb fc");
}

fn void test_write_30_bits() {
	ByteWriter w;
	w.temp_init();

	BitWriter bw;
	bw.init(&w);

	bw.write_bits(0xf0f1f2f3, 30)!!; // 11 | 110000111100011111001011110011
	bw.flush()!!;
											//    c    3    c    7    c    b    c    c
	assert(w.str_view() == x"c3 c7 cb cc"); // 1100 0011 1100 0111 1100 1011 1100 1100
}

fn void test_write_32_bits() {
	ByteWriter w;
	w.temp_init();

	BitWriter bw;
	bw.init(&w);

	bw.write_bits(0xfafbfcfd, 32)!!;
	bw.flush()!!;

	assert(w.str_view() == x"fa fb fc fd");
}

fn void test_write_2_bits_multiple() {
	ByteWriter w;
	w.temp_init();

	BitWriter bw;
	bw.init(&w);

	bw.write_bits(0b11111111, 2)!!;
	bw.write_bits(0b00000001, 2)!!;
	bw.write_bits(0b11111111, 2)!!;
	bw.flush()!!;

	assert(w.str_view() == x"dc"); // 0b1101 1100
}

fn void test_write_10_bits_multiple() {
	ByteWriter w;
	w.temp_init();

	BitWriter bw;
	bw.init(&w);

	bw.write_bits(0x789, 10)!!; // 01 | 11 1000 1001
	bw.write_bits(0xabc, 10)!!; // 10 | 10 1011 1100
	bw.write_bits(0xdef, 10)!!; // 11 | 01 1110 1111
	bw.flush()!!;

										    //      e    2    6    b    c    7    b    c
	assert(w.str_view() == x"e2 6b c7 bc"); // 0b1110 0010 0110 1011 1100 0111 1011 1100
}

fn void test_write_24_bits_multiple() {
	ByteWriter w;
	w.temp_init();

	BitWriter bw;
	bw.init(&w);

	bw.write_bits(0xfafbfc, 24)!!;
	bw.write_bits(0xfdfeff, 24)!!;
	bw.flush()!!;

	assert(w.str_view() == x"fa fb fc fd fe ff");
}

fn void test_write_30_bits_multiple() {
	ByteWriter w;
	w.temp_init();

	BitWriter bw;
	bw.init(&w);

	bw.write_bits(0xf0f1f2f3, 30)!!; // 11 | 110000111100011111001011110011
	bw.write_bits(0xfafbfcfd, 30)!!; // 11 | 111010111110111111110011111101
	bw.flush()!!;

	assert(w.str_view() == x"c3 c7 cb cf af bf cf d0");
}

fn void test_write_32_bits_multiple() {
	ByteWriter w;
	w.temp_init();

	BitWriter bw;
	bw.init(&w);

	bw.write_bits(0xf0f1f2f3, 32)!!;
	bw.write_bits(0xfafbfcfd, 32)!!;
	bw.flush()!!;

	assert(w.str_view() == x"f0 f1 f2 f3 fa fb fc fd");
}

fn void test_write_mixed_multiple() {
	ByteWriter w;
	w.temp_init();

	BitWriter bw;
	bw.init(&w);

	bw.write_bits(0xf0f1f2f3, 8)!!;
	bw.write_bits(0xf0f1f2f3, 32)!!;
	bw.write_bits(0xfafbfcfd, 30)!!;
	bw.write_bits(0xf4f5f6f7, 10)!!;
	bw.flush()!!;

	assert(w.str_view() == x"f3 f0 f1 f2 f3 eb ef f3 f6 f7");
}