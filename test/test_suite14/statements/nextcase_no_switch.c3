module test;
fn void foo()
{
	nextcase 3; // #error: No matching switch could be found
}

fn int foo2(double b, int x)
{
	switch (x)
	{
		case 1:
			nextcase 2;
		case 2:
			break;
		case 3:
			defer { nextcase 2; } // #error: No matching switch could be found
	}
    return 1;
}

fn int foo3(double b, int x)
{
	switch FOO: (x)
	{
		case 2:
			break;
		case 3:
			{| nextcase FOO: 2; |}; // #error: You cannot use nextcase to exit
	}
    return 1;
}