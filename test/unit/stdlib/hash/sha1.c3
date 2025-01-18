module std::hash::sha1_test @test;
import std::hash::sha1;

fn void test_sha1_abc()
{
	Sha1 sha;
	sha.init();
	sha.update("abc");
	assert(sha.final() == x"A9993E36 4706816A BA3E2571 7850C26C 9CD0D89D");
}

fn void test_sha1_even_longer()
{
	char[] content = "Lorem ipsum dolor sit amet, consectetur adipiscing elit. Fusce viverra nulla nec neque convallis feugiat. Vestibulum sodales at.";
	Sha1 sha1;
	sha1.init();
	sha1.update(content);
	assert(sha1.final() == x"7ba51bc42c21e6159556537c2134d1c2028799ef");
}

fn void test_sha1_longer()
{
	Sha1 sha;
	sha.init();
	sha.update("abcdbcdecdefdefgefghfghighijhijkijkljklmklmnlmnomnopnopq");
	assert(sha.final() == x"84983E44 1C3BD26E BAAE4AA1 F95129E5 E54670F1");
}

fn void test_pbkdf2()
{
	char[] pw = "password";
	char[] s = "salt";
	char[20] out;
	sha1::pbkdf2(pw, s, 1, &out);
	assert(out == x'0c60c80f961f0e71f3a9b524af6012062fe037a6');
	sha1::pbkdf2(pw, s, 2, &out);
	assert(out == x'ea6c014dc72d6f8ccd1ed92ace1d41f0d8de8957');
	sha1::pbkdf2(pw, s, 4096, &out);
	assert(out == x'4b007901b765489abead49d926f721d065a429c1');
}

fn void test_pbkdf2_2()
{
	char[] pw = "passwordPASSWORDpassword";
	char[] s = "saltSALTsaltSALTsaltSALTsaltSALTsalt";
	char[25] out;
	sha1::pbkdf2(pw, s, 4096, &out);
	assert(out == x'3d2eec4fe41c849b80c8d83662c0e44a8b291a964cf2f07038');
}

fn void test_pbkdf2_3()
{
	char[] pw = "pass\0word";
	char[] salt = "sa\0lt";
	char[16] out;
	sha1::pbkdf2(pw, salt, 4096, &out);
	assert(out == x'56fa6aa75548099dcc37d7f03425e0c3');
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