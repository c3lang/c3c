module std::hash::sha512_test @test;
import std::hash::sha512;

fn void test_sha512_empty()
{
    Sha512 sha;
    sha.init();
    sha.update("");
    assert(sha.final() == x"cf83e1357eefb8bdf1542850d66d8007d620e4050b5715dc83f4a921d36ce9ce47d0d13c5d85f2b0ff8318d2877eec2f63b931bd47417a81a538327af927da3e");
}

fn void test_sha512_abc()
{
    Sha512 sha;
    sha.init();
    sha.update("abc");
    assert(sha.final() == x"ddaf35a193617abacc417349ae20413112e6fa4e89a97ea20a9eeee64b55d39a2192992a274fc1a836ba3c23a3feebbd454d4423643ce80e2a9ac94fa54ca49f");
}

fn void test_sha512_longer()
{
    Sha512 sha;
    sha.init();
    sha.update("abcdbcdecdefdefgefghfghighijhijkijkljklmklmnlmnomnopnopqabcdbcdecdefdefgefghfghighijhijkijkljklmklmnlmnomnopnopq");
    assert(sha.final() == x"7361ec4a617b6473fb751c44d1026db9442915a5fcea1a419e615d2f3bc5069494da28b8cf2e4412a1dc97d6848f9c84a254fb884ad0720a83eaa0434aeafd8c");
}

/*
fn void gigahash_sha512()
{
    // This test is here because of an overflow that was present in SHA256.
    char[] c = calloc(257 * (1024*1024))[:(257*1024*1024)];
    defer free(c);

    Sha512 sha;
    sha.init();
    sha.update(c);
    assert(sha.final() == x"724999ca733fdd49f489fc59a49ce41460531a78aa0c03488be9f4a1e29f955bd325d47462c89234a8f587d8a5d6a9ab79137bc5ce3acbfb928553dba8431a36");
}
*/
fn void test_pbkdf2()
{
    char[] pw = "password";
    char[] s = "salt";
    char[sha512::HASH_SIZE] out;
    sha512::pbkdf2(pw, s, 1, &out);
    assert(out == x'867f70cf1ade02cff3752599a3a53dc4af34c7a669815ae5d513554e1c8cf252c02d470a285a0501bad999bfe943c08f050235d7d68b1da55e63f73b60a57fce');
    sha512::pbkdf2(pw, s, 2, &out);
    assert(out == x'e1d9c16aa681708a45f5c7c4e215ceb66e011a2e9f0040713f18aefdb866d53cf76cab2868a39b9f7840edce4fef5a82be67335c77a6068e04112754f27ccf4e');
    sha512::pbkdf2(pw, s, 4096, &out);
    assert(out == x'd197b1b33db0143e018b12f3d1d1479e6cdebdcc97c5c0f87f6902e072f457b5143f30602641b3d55cd335988cb36b84376060ecd532e039b742a239434af2d5');
}

fn void test_pbkdf2_2()
{
    char[] pw = "passwordPASSWORDpassword";
    char[] s = "saltSALTsaltSALTsaltSALTsaltSALTsalt";
    char[sha512::HASH_SIZE] out;
    sha512::pbkdf2(pw, s, 4096, &out);
    assert(out == x'8c0511f4c6e597c6ac6315d8f0362e225f3c501495ba23b868c005174dc4ee71115b59f9e60cd9532fa33e0f75aefe30225c583a186cd82bd4daea9724a3d3b8');
}

fn void test_pbkdf2_3()
{
    char[] pw = "pass\0word";
    char[] salt = "sa\0lt";
    char[sha512::HASH_SIZE] out;
    sha512::pbkdf2(pw, salt, 4096, &out);
    assert(out == x'9d9e9c4cd21fe4be24d5b8244c759665f39d98fc12a9ca759bb021db3cfadf345844aebe70dd8b2f6966f25f3613e1187bbd24ed2ca43ed13b246e4675be7ab9');
}

fn void test_sha512_million_a()
{
    Sha512 sha;
    sha.init();
    const int COUNT = 1_000_000;
    for (int i = 0; i < COUNT / 10; i++)
    {
        sha.update("aaaaaaaaaa");
    }
    assert(sha.final() == x"e718483d0ce769644e2e42c7bc15b4638e1f98b13b2044285632a803afa973ebde0ff244877ea60a4cb0432ce577c31beb009c5c2c49aa2e4eadb217ad8cc09b");
}

