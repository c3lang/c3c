module testing;

struct Foo
{
	Entry[1 << N] data; // #error: 'N' could not be found
}

struct Entry
{
    String key;
    String value;
}