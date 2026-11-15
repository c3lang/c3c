module typedef_element_vec_cast;

typedef MyInt = int;
typedef MyF = float;
const MyInt[] MS = { (MyInt)7, (MyInt)-8 };
const MyInt[2] MA = { (MyInt)7, (MyInt)-8 };

fn void typedef_element_vec_cast() @test
{
	$assert ((int[<2>])MS)[1] == -8;
	$assert ((int[<2>])MA)[1] == -8;
	MyInt[] s = { (MyInt)1, (MyInt)-2 };
	MyInt[2] a = { (MyInt)3, (MyInt)-4 };
	MyInt[<2>] v = { (MyInt)3, (MyInt)-4 };
	MyF[<2>] f = { (MyF)1.5, (MyF)-2.5 };
	assert((int[<2>])s[:2] == { 1, -2 });
	assert((long[<2>])a == { 3, -4 });
	assert((long[<2>])v == { 3, -4 });
	assert((float[<2>])v == { 3.0f, -4.0f });
	assert((int[<2>])f == { 1, -2 });
	assert((double[<2>])f == { 1.5, -2.5 });
}
