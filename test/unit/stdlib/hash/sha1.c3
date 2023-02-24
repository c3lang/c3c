module std::hash::sha1_test @test;
import std::hash::sha1;

fn void test_sha1_abc()
{
	Sha1 sha;
	sha.init();
	sha.update("abc");
	assert(sha.final() == x"A9993E36 4706816A BA3E2571 7850C26C 9CD0D89D");
}

fn void test_sha1_longer()
{
	Sha1 sha;
	sha.init();
	sha.update("abcdbcdecdefdefgefghfghighijhijkijkljklmklmnlmnomnopnopq");
	assert(sha.final() == x"84983E44 1C3BD26E BAAE4AA1 F95129E5 E54670F1");
}

fn void test_sha1_million_a()
{
	Sha1 sha;
	sha.init();
	const int COUNT = 1_000_000;
	for (int i = 0; i < COUNT / 10; i++)
	{
		sha.update("aaaaaaaaaa");
	}
	assert(sha.final() == x"34AA973C D4C4DAA4 F61EEB2B DBAD2731 6534016F");
}