module std::hash::sha256_test @test;
import std::hash::sha256;

fn void test_sha256_empty()
{
    Sha256 sha;
    sha.init();
    sha.update("");

    test::@check(sha.final() == x"E3B0C442 98FC1C14 9AFBF4C8 996FB924 27AE41E4 649B934C A495991B 7852B855");
}

fn void test_sha256_abc()
{
    Sha256 sha;
    sha.init();
    sha.update("abc");

    test::@check(sha.final() == x"BA7816BF 8F01CFEA 414140DE 5DAE2223 B00361A3 96177A9C B410FF61 F20015AD");
}

fn void test_sha256_longer()
{
    Sha256 sha;
    sha.init();
    sha.update("abcdbcdecdefdefgefghfghighijhijkijkljklmklmnlmnomnopnopqabcdbcdecdefdefgefghfghighijhijkijkljklmklmnlmnomnopnopq");
    test::@check(sha.final() == x"59F109D9 533B2B70 E7C3B814 A2BD218F 78EA5D37 14455BC6 7987CF0D 664399CF");
}

fn void test_sha256_multi_update_permute()
{
    char[] input = "a really long string a really long string a really long string a really long string a really long string a really long string a really long string a really long string a really long string a really long string a really long string a really long string a really long string a really long string a really long string a really long string a really long string a really long string a really long string a really long string a really long string a really long string a really long string a really long string";
    for (usz step = 1; step < input.len; step++)
    {
        Sha256 sha;
        sha.init();
        usz i = 0;
        for (; i < input.len / step; i++) sha.update(input[i*step : step]);
        if (i*step < input.len) sha.update(input[i*step..]);
        test::@check(sha.final() == x"b527293dfb70dcce37e593f4c43e1b81909615722bad041b90b8df22bebd00a0", "Mismatch for step %d", step);
    }
}

/*
fn void gigahash_sha256()
{
    // > 256 MiB is just at the threshold where the SHA bitcounter rolls overflows 'len', but doesn't hit the uint.max limit...
    char[] c = calloc(257 * (1024*1024))[:(257*1024*1024)];
    defer free(c);

    Sha256 sha;
    sha.init();
    sha.update(c);
    test::@check(sha.final() == x"053EADFD EC682CF1 6F3F8704 C7609C57 868DD757 65E08DC5 A7491F5D 06BCB74D");
}
*/
fn void test_pbkdf2()
{
    char[] pw = "password";
    char[] s = "salt";
    char[32] out;
    sha256::pbkdf2(pw, s, 1, &out);
    test::@check(out == x'120FB6CF FCF8B32C 43E72252 56C4F837 A86548C9 2CCC3548 0805987C B70BE17B');
    sha256::pbkdf2(pw, s, 2, &out);
    test::@check(out == x'AE4D0C95 AF6B46D3 2D0ADFF9 28F06DD0 2A303F8E F3C251DF D6E2D85A 95474C43');
    sha256::pbkdf2(pw, s, 4096, &out);
    test::@check(out == x'C5E478D5 9288C841 AA530DB6 845C4C8D 962893A0 01CE4E11 A4963873 AA98134A');
}

fn void test_pbkdf2_2()
{
    char[] pw = "passwordPASSWORDpassword";
    char[] s = "saltSALTsaltSALTsaltSALTsaltSALTsalt";
    char[32] out;
    sha256::pbkdf2(pw, s, 4096, &out);
    test::@check(out == x'348C89DB CBD32B2F 32D814B8 116E84CF 2B17347E BC180018 1C4E2A1F B8DD53E1');
}


fn void test_pbkdf2_3()
{
    char[] pw = "pass\0word";
    char[] salt = "sa\0lt";
    char[32] out;
    sha256::pbkdf2(pw, salt, 4096, &out);

    test::@check(out == x'89B69D05 16F82989 3C696226 650A8687 8C029AC1 3EE27650 9D5AE58B 6466A724');
}

fn void test_sha256_million_a()
{
    Sha256 sha;
    sha.init();
    const int COUNT = 1_000_000;
    for (int i = 0; i < COUNT / 10; i++)
    {
        sha.update("aaaaaaaaaa");
    }
    test::@check(sha.final() == x"CDC76E5C 9914FB92 81A1C7E2 84D73E67 F1809A48 A497200E 046D39CC C7112CD0");
}

fn void test_rfc_4231_4_2_test_case_1() @test {
    char[20] key;
    for (uint i = 0; i < key.len; i += 1) key[i] = 0x0b;
    const char[] MSG = "Hi There";
    const char[] EXPECTED =
        x"b0344c61d8db38535ca8afceaf0bf12b"
        x"881dc200c9833da726e9376c2e32cff7";
    char[sha256::HASH_SIZE] mac = sha256::hmac(&key, MSG);
    test::@check(
        &mac == EXPECTED,
        "Expected %h, got %h",
        EXPECTED,
        mac
    );
}

fn void test_rfc_4231_4_3_test_case_2() @test {
    const char[] KEY = "Jefe";
    const char[] MSG = "what do ya want for nothing?";
    const char[] EXPECTED =
        x"5bdcc146bf60754e6a042426089575c7"
        x"5a003f089d2739839dec58b964ec3843";
    char[sha256::HASH_SIZE] mac = sha256::hmac(KEY, MSG);
    test::@check(
        &mac == EXPECTED,
        "Expected %h, got %h",
        EXPECTED,
        mac
    );
}

fn void test_rfc_4231_4_4_test_case_3() @test {
    char[20] key;
    for (uint i = 0; i < key.len; i += 1) key[i] = 0xaa;
    char[50] msg;
    for (uint i = 0; i < msg.len; i += 1) msg[i] = 0xdd;
    const char[] EXPECTED =
        x"773ea91e36800e46854db8ebd09181a7"
        x"2959098b3ef8c122d9635514ced565fe";
    char[sha256::HASH_SIZE] mac = sha256::hmac(&key, &msg);
    test::@check(
        &mac == EXPECTED,
        "Expected %h, got %h",
        EXPECTED,
        mac
    );
}

fn void test_rfc_4231_4_5_test_case_4() @test {
    char[25] key;
    for (uint i = 0; i < key.len; i += 1) key[i] = (char)(i + 1); // 0x01..0x19
    char[50] msg;
    for (uint i = 0; i < msg.len; i += 1) msg[i] = 0xcd;
    const char[] EXPECTED =
        x"82558a389a443c0ea4cc819899f2083a"
        x"85f0faa3e578f8077a2e3ff46729665b";
    char[sha256::HASH_SIZE] mac = sha256::hmac(&key, &msg);
    test::@check(
        &mac == EXPECTED,
        "Expected %h, got %h",
        EXPECTED,
        mac
    );
}

// RFC 4231 4.6 Test Case 5 ignored
// hmac method from sha256 module does not support truncated output
// please truncate it yourself

fn void test_rfc_4231_4_7_test_case_6() @test {
    char[131] key;
    for (uint i = 0; i < key.len; i += 1) key[i] = 0xaa;
    const char[] MSG =
        "Test Using Larger Than Block-Size Key - Hash Key First";
    const char[] EXPECTED =
        x"60e431591ee0b67f0d8a26aacbf5b77f"
        x"8e0bc6213728c5140546040f0ee37f54";
    char[sha256::HASH_SIZE] mac = sha256::hmac(&key, MSG);
    test::@check(
        &mac == EXPECTED,
        "Expected %h, got %h",
        EXPECTED,
        mac
    );
}

fn void test_rfc_4231_4_8_test_case_7() @test {
    char[131] key;
    for (uint i = 0; i < key.len; i += 1) key[i] = 0xaa;
    const char[] MSG =
        "This is a test using a larger than block-size key and a larger "
        "than block-size data. The key needs to be hashed before being "
        "used by the HMAC algorithm.";
    const char[] EXPECTED =
        x"9b09ffa71b942fcb27635fbcd5b0e944"
        x"bfdc63644f0713938a7f51535c3a35e2";
    char[sha256::HASH_SIZE] mac = sha256::hmac(&key, MSG);
    test::@check(
        &mac == EXPECTED,
        "Expected %h, got %h",
        EXPECTED,
        mac
    );
}

