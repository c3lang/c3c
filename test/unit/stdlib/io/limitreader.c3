module std::io @test;


fn void limitreader()
{
    const DATA = "Hello World!";
    ByteReader src;
    src.init(DATA);
    const LIMIT = 5;
    LimitReader lmr;
    lmr.init(&src, LIMIT);

    char[DATA.len] bytes;
    usz n = lmr.read(bytes[..])!!;

    assert(n == LIMIT, "got %d; want %d", n, LIMIT);
    String got = (String)bytes[:n];
    String want = DATA[:LIMIT];
    assert(got == want, "got %d; want %d", got, want);
}