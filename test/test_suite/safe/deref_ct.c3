module test;
macro void test()
{
    any *$value;
    typeid t = $value.type; // #error: This value is known to be null so you
}

fn int main()
{
	test();
	return 0;
}