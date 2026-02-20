module std::core::bitorder @test;

fn void test_unaligned()
{
	usz file_size;
	char[64] response;
	bitorder::write(file_size, response[9..], ULongLE);
}

fn void test_read()
{
    char[*] bytes = { 0x01, 0x02, 0x03, 0x04, 0x05, 0x06, 0x07, 0x08 };

    assert(bitorder::read(bytes, UShortBE) == 0x0102);
    assert(bitorder::read(bytes, UShortLE) == 0x0201);
    assert(bitorder::read(bytes, UIntBE) == 0x01020304);
    assert(bitorder::read(bytes, UIntLE) == 0x04030201);
    assert(bitorder::read(bytes, ULongBE) == 0x0102030405060708);
    assert(bitorder::read(bytes, ULongLE) == 0x0807060504030201);
    
    assert(bitorder::read(&bytes, UShortBE) == 0x0102);
    assert(bitorder::read(&bytes, UShortLE) == 0x0201);
    assert(bitorder::read(&bytes, UIntBE) == 0x01020304);
    assert(bitorder::read(&bytes, UIntLE) == 0x04030201);
    assert(bitorder::read(&bytes, ULongBE) == 0x0102030405060708);
    assert(bitorder::read(&bytes, ULongLE) == 0x0807060504030201);

    assert(bitorder::read(bytes[..], UShortBE) == 0x0102);
    assert(bitorder::read(bytes[..], UShortLE) == 0x0201);
    assert(bitorder::read(bytes[..], UIntBE) == 0x01020304);
    assert(bitorder::read(bytes[..], UIntLE) == 0x04030201);
    assert(bitorder::read(bytes[..], ULongBE) == 0x0102030405060708);
    assert(bitorder::read(bytes[..], ULongLE) == 0x0807060504030201);
}

fn void test_load()
{
    char[*] bytes = { 0x01, 0x02, 0x03, 0x04, 0x05, 0x06, 0x07, 0x08 };
	assert(bitorder::load_be((short*)&bytes) == 0x0102);
    assert(bitorder::load_le((short*)&bytes) == 0x0201);
    assert(bitorder::load_be((long*)&bytes) == 0x0102030405060708);
    assert(bitorder::load_le((long*)&bytes) == 0x0807060504030201);
}

fn void test_write()
{
    char[*] bytes = { 0x01, 0x02, 0x03, 0x04, 0x05, 0x06, 0x07, 0x08 };
    char[8] buf;

    ushort x1 = bitorder::read(bytes, UShortBE);
    bitorder::write(x1, &buf, UShortBE);
    assert(bitorder::read(buf, UShortBE) == x1);

    uint x2 = bitorder::read(bytes, UIntBE);
    bitorder::write(x2, buf[..], UIntBE);
    assert(bitorder::read(buf, UIntBE) == x2);

    ulong x3 = bitorder::read(bytes, ULongBE);
    bitorder::write(x3, buf[..], ULongBE);
    assert(bitorder::read(buf, ULongBE) == x3);
}

fn void test_store()
{
    char[*] bytes = { 0x01, 0x02, 0x03, 0x04, 0x05, 0x06, 0x07, 0x08 };
    char[8] buf;

    ushort x1 = bitorder::load_be((ushort*)&bytes);
    bitorder::store_be(&buf, x1);
    assert(bitorder::read(buf, UShortBE) == x1);

    uint x2 = bitorder::load_be((uint*)&bytes);
    bitorder::store_be(&buf, x2);
    assert(bitorder::read(buf, UIntBE) == x2);

    ulong x3 = bitorder::load_be((ulong*)&bytes);
    bitorder::store_be(&buf, x3);
    assert(bitorder::read(buf, ULongBE) == x3);
}

fn void test_store_le()
{
    char[*] bytes = { 0x01, 0x02, 0x03, 0x04, 0x05, 0x06, 0x07, 0x08 };
    char[8] buf;

    ushort x1 = bitorder::load_le((ushort*)&bytes);
    bitorder::store_le(&buf, x1);
    assert(bitorder::read(buf, UShortLE) == x1);

    uint x2 = bitorder::load_le((uint*)&bytes);
    bitorder::store_le(&buf, x2);
    assert(bitorder::read(buf, UIntLE) == x2);

    ulong x3 = bitorder::load_le((ulong*)&bytes);
    bitorder::store_le(&buf, x3);
    assert(bitorder::read(buf, ULongLE) == x3);

}