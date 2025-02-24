module std::hash::fnv64a_test @test;
import std::hash::fnv64a;

fn void test_fnv64a()
{
	Fnv64a hash;

	char[] input = "Hello world";
	ulong want = 0x2713f785a33764c7;

	// update
	hash.init();
	hash.update(input);
	assert ((ulong)hash == want, "got: %d, want: %d", hash, want);

	// update_char
	hash.init();
	foreach (c : input) hash.update_char(c);
	assert ((ulong)hash == want, "got: %d, want: %d", hash, want);

	// encode
	ulong encoded = fnv64a::encode(input);
	assert (encoded == want, "got: %d, want: %d", encoded, want);
}
