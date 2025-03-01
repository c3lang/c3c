module std::hash::md5_test @test;
import std::hash::md5;


fn void test_md5_rfc()
{
	Md5 md5;
	md5.init();
	assert (md5.final() == x'd41d8cd98f00b204e9800998ecf8427e');

	md5.init();
	md5.update("a");
	assert (md5.final() == x'0cc175b9c0f1b6a831c399e269772661');

	md5.init();
	md5.update("abc");
	assert (md5.final() == x'900150983cd24fb0d6963f7d28e17f72');

	md5.init();
	md5.update("message ");
	md5.update("digest");
	assert(md5.final() == x'f96b697d7cb7938d525a2f31aaf161d0');

	md5.init();
	md5.update("abcdefghijklmnopqrstuvwxyz");
	assert(md5.final() == x'c3fcd3d76192e4007dfb496cca67e13b');

	md5.init();
	md5.update("ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789");
	assert(md5.final() == x'd174ab98d277d9f5a5611c2c9f419d9f');

	md5.init();
	md5.update("12345678901234567890123456789012345678901234567890123456789012345678901234567890");
	assert(md5.final() == x'57edf4a22be3c955ac49da2e2107b67a');

}

fn void test_md5_hash()
{
	assert(md5::hash("12345678901234567890123456789012345678901234567890123456789012345678901234567890") == x'57edf4a22be3c955ac49da2e2107b67a');
}
