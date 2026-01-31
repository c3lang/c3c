module testing_branch;
faultdef FOO;

macro usz? problemz(val) => val >= 5 ? usz : FOO~; // #error: Implicitly casting 'typeid' to 'usz'
macro usz? problemz2(val) => val >= 5 ? $typefrom("usz") : FOO~; // #error: Implicitly casting 'typeid' to 'usz'

fn int main()
{
	for (usz i = 0; i < 10; i++)
	{
		problemz(i)!!;
		problemz2(i)!!;
	}
	return 0;
}