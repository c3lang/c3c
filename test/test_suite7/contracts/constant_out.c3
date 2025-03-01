module test;


struct Abc
{
	int a;
}

fn void Abc.update(Abc* a, int ab)
{
	a.a = ab;
}

<*
 @param [out] a
*>
fn void Abc.update_fail(Abc* a, int ab)
{
	a.a = ab;
}

const Abc X = { 2 };


fn int main()
{
	const Abc Y = { 3 };
	int* z = &X.a;
	*z = 31;
	X.update(1);
	X.update_fail(2); // #error: const parameter may not
	X.a = 32; // #error: cannot assign to a constant
	Y.a = 34; // #error: cannot assign to a constant
	return 0;
}

