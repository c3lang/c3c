module std::io @test;

const DATA = "Lorem ipsum blandit.";

fn void! readbuffer_large()
{
    ByteReader src;
    src.init(DATA);
    char[DATA.len] buf;
    ReadBuffer reader_buf;
    reader_buf.init(&src, buf[..]);

    char[DATA.len] bytes;
    usz n = reader_buf.read(bytes[..])!;

    assert(n == DATA.len, "large read failed: got %d; want %d", n, DATA.len);
    String got = (String)bytes[..];
    assert(got == DATA, "large read failed: got %d; want %d", got, DATA);
}

fn void! readbuffer()
{
    ByteReader src;
    src.init(DATA);
    char[3] buf;
    ReadBuffer reader_buf;
    reader_buf.init(&src, buf[..]);

    ByteWriter bw;
    bw.tinit();

    usz n = reader_buf.copy_to(&bw)!;

    assert(n == DATA.len, "got %d; want %d", n, DATA.len);
    String got = bw.str_view();
    assert(got == DATA, "got %d; want %d", got, DATA);
}

fn void! writebuffer_large()
{
    ByteWriter out;
    out.tinit();
    char[16] buf;
    WriteBuffer write_buf;
    write_buf.init(&out, buf[..]);

    usz n = write_buf.write(DATA)!;

    assert(n == DATA.len, "large write failed: got %d; want %d", n, DATA.len);
    String got = out.str_view();
    assert(got == DATA, "large write failed: got %d; want %d", got, DATA);
}

fn void! writebuffer()
{
    ByteReader br;
    br.init(DATA);
    ByteWriter out;
    out.tinit();
    char[3] buf;
    WriteBuffer write_buf;
    write_buf.init(&out, buf[..]);

    usz n = br.copy_to(&write_buf)!;

    assert(n == DATA.len, "got %d; want %d", n, DATA.len);
    String got = out.str_view();
    assert(got == DATA, "got %d; want %d", got, DATA);
}