fn $typeof({})? foo() {} // #error: A function may not return an optional with a compile time type, only macros may do

fn int main()
{
	return 0;
}