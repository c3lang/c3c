module main;

struct OwnedString
{
	usz len;
	char[*] data;
}

fn void test1()
{
	OwnedString owned;
	owned.data = {}; // #error: You can't assign to a flexible array member
}

fn void test2()
{
	$typefrom(OwnedString.data.typeid) x = 1; // #error:  'char[*]' has unknown size, and
}

fn int main()
{
	OwnedString owned;
	owned.data[0] = 1;
    return 0;
}