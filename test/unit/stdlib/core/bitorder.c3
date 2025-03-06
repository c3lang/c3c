module std::core::bitorder @test;

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