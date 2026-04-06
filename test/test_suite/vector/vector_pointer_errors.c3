module vecpointer;

fn void pointer_add_sub_diff()
{
	int[5] a;
	int*[<2>] y;
    double*[<2>] z = y; // #error: 'int*[<2>]' to 'double*[<2>]'
}

fn void pointer_add_sub_diff2()
{
	int[5] a;
	int*[<2>] y;
	y / y; // #error: Cannot divide
}

fn void pointer_add_sub_diff3()
{
	int[5] a;
	int*[<2>] y;
	y % y; // #error: Cannot calculate the remainder
}

fn void pointer_add_sub_diff4()
{
	int[5] a;
	int*[<2>] y;
	y * y; // #error: multiply
}

fn void pointer_add_sub_diff5()
{
	int[5] a;
	int*[<2>] y;
	y ^ y; // #error: not defined
}

fn void pointer_add_sub_diff6()
{
	int[5] a;
	int*[<2>] y;
	iptr[<2>] g = (iptr[<2>])y;
	g | g;
}
