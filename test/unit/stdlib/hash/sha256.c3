module std::hash::sha256_test @test;
import std::hash::sha256;

fn void test_sha256_empty()
{
    Sha256 sha;
    sha.init();
    sha.update("");

    assert(sha.final() == x"E3B0C442 98FC1C14 9AFBF4C8 996FB924 27AE41E4 649B934C A495991B 7852B855");
}

fn void test_sha256_abc()
{
    Sha256 sha;
    sha.init();
    sha.update("abc");

    assert(sha.final() == x"BA7816BF 8F01CFEA 414140DE 5DAE2223 B00361A3 96177A9C B410FF61 F20015AD");
}

fn void test_sha256_longer()
{
    Sha256 sha;
    sha.init();
    sha.update("abcdbcdecdefdefgefghfghighijhijkijkljklmklmnlmnomnopnopqabcdbcdecdefdefgefghfghighijhijkijkljklmklmnlmnomnopnopq");
    assert(sha.final() == x"59F109D9 533B2B70 E7C3B814 A2BD218F 78EA5D37 14455BC6 7987CF0D 664399CF");
}

fn void test_pbkdf2()
{
    char[] pw = "password";
    char[] s = "salt";
    char[32] out;
    sha256::pbkdf2(pw, s, 1, &out);
    assert(out == x'120FB6CF FCF8B32C 43E72252 56C4F837 A86548C9 2CCC3548 0805987C B70BE17B');
    sha256::pbkdf2(pw, s, 2, &out);
    assert(out == x'AE4D0C95 AF6B46D3 2D0ADFF9 28F06DD0 2A303F8E F3C251DF D6E2D85A 95474C43');
    sha256::pbkdf2(pw, s, 4096, &out);
    assert(out == x'C5E478D5 9288C841 AA530DB6 845C4C8D 962893A0 01CE4E11 A4963873 AA98134A');
}

fn void test_pbkdf2_2()
{
    char[] pw = "passwordPASSWORDpassword";
    char[] s = "saltSALTsaltSALTsaltSALTsaltSALTsalt";
    char[32] out;
    sha256::pbkdf2(pw, s, 4096, &out);
    assert(out == x'348C89DB CBD32B2F 32D814B8 116E84CF 2B17347E BC180018 1C4E2A1F B8DD53E1');
}


fn void test_pbkdf2_3()
{
    char[] pw = "pass\0word";
    char[] salt = "sa\0lt";
    char[32] out;
    sha256::pbkdf2(pw, salt, 4096, &out);

    assert(out == x'89B69D05 16F82989 3C696226 650A8687 8C029AC1 3EE27650 9D5AE58B 6466A724');
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
   
    assert(sha.final() == x"CDC76E5C 9914FB92 81A1C7E2 84D73E67 F1809A48 A497200E 046D39CC C7112CD0");
}