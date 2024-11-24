module encoding::base64 @test;
import std::encoding::base64;

// https://www.rfc-editor.org/rfc/rfc4648#section-10

struct TestCase
{
    char[] in;
    char[] out;
}

fn void encode()
{
    TestCase[] tcases = {
        { "", "" },
        { "f", "Zg==" },
        { "fo", "Zm8=" },
        { "foo", "Zm9v" },
        { "foob", "Zm9vYg==" },
        { "fooba", "Zm9vYmE=" },
        { "foobar", "Zm9vYmFy" },
        { "test", "dGVzdA==" },
    };
    foreach (tc : tcases)
    {
        @pool()
        {
            Base64Encoder b;
            b.init(base64::STD_ALPHABET)!;
            usz n = b.encode_len(tc.in.len);
            char[64] buf;
            b.encode(tc.in, buf[:n])!;
            assert(buf[:n] == tc.out);
            assert(base64::encode_temp(tc.in) == tc.out);
        };
    }
}

fn void encode_nopadding()
{
    TestCase[] tcases = {
        { "", "" },
        { "f", "Zg" },
        { "fo", "Zm8" },
        { "foo", "Zm9v" },
        { "foob", "Zm9vYg" },
        { "fooba", "Zm9vYmE" },
        { "foobar", "Zm9vYmFy" },
        { "test", "dGVzdA" },
    };
    foreach (tc : tcases)
    {
        Base64Encoder b;
        b.init(base64::STD_ALPHABET, -1)!;
        usz n = b.encode_len(tc.in.len);
        char[64] buf;
        b.encode(tc.in, buf[:n])!;
        assert(buf[:n] == tc.out);
    }
}

fn void decode()
{
    TestCase[] tcases = {
        { "", "" },
        { "Zg==", "f" },
        { "Zm8=", "fo" },
        { "Zm9v", "foo" },
        { "Zm9vYg==", "foob" },
        { "Zm9vYmE=", "fooba" },
        { "Zm9vYmFy", "foobar" },
        { "Zm9vYmFy", "foobar" },
        { "dGVzdA==", "test" },
    };
    foreach (tc : tcases)
    {
        Base64Decoder b;
        b.init(base64::STD_ALPHABET)!;
        usz n = b.decode_len(tc.in.len)!;
        char[64] buf;
        usz nn = b.decode(tc.in, buf[:n])!;
        assert(buf[:nn] == tc.out);
    }
}

fn void decode_nopadding()
{
    TestCase[] tcases = {
        { "", "" },
        { "Zg", "f" },
        { "Zm8", "fo" },
        { "Zm9v", "foo" },
        { "Zm9vYg", "foob" },
        { "Zm9vYmE", "fooba" },
        { "Zm9vYmFy", "foobar" },
        { "dGVzdA", "test" },
    };
    foreach (tc : tcases)
    {
        Base64Decoder b;
        b.init(base64::STD_ALPHABET, -1)!;
        usz n = b.decode_len(tc.in.len)!;
        char[64] buf;
        usz nn = b.decode(tc.in, buf[:n])!;
        assert(buf[:nn] == tc.out);
    }
}
