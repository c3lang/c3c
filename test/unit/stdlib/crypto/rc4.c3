import std::crypto;
import std::io;

fn void rc_crypt() @test
{
	Rc4 rc;
	rc.init(&&x"63727970746969");
	char[200] x;
	String text = "The quick brown fox jumps over the lazy dog.";
	rc.crypt(text, &x);
	char[*] res = x'2ac2fecdd8fbb84638e3a4
	                820eb205cc8e29c28b9d5d
	                6b2ef974f311964971c90e
	                8b9ca16467ef2dc6fc3520';
	assert(res[:text.len] == x[:text.len]);
}