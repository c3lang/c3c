module vecpointer;

fn void pointer_add_sub_diff()
{
	int[5] a;
	int*[<2>] y;
    double*[<2>] z = y; // #error: 'int*[<2>]' to 'double*[<2>]'
	y / y; // #error: Cannot divide
	y % y; // #error: Cannot calculate the remainder
	y * y; // #error: multiply
	y ^ y; // #error: not defined
	iptr[<2>] g = (iptr[<2>])y;
	g | g;
}
