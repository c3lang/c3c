module std::hash::fnv32a_test @test;
import std::hash::fnv32a;

fn void test_fnv32a()
{
	Fnv32a hash;

	char[] input = "Hello world";
	uint want = 0x594d29c7;

	// update
	hash.init();
	hash.update(input);
	assert ((uint)hash == want, "got: %d, want: %d", hash, want);

	// update_char
	hash.init();
	foreach (c : input) hash.update_char(c);
	assert ((uint)hash == want, "got: %d, want: %d", hash, want);

	// encode
	uint encoded = fnv32a::encode(input);
	assert (encoded == want, "got: %d, want: %d", encoded, want);
}
