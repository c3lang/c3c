enum Foo { A, B, C }

fn int runtime_case(Foo e, Foo x) // #error: Missing return statement
{
	switch (e)
	{
		case x: return 1;
		case B: return 2;
		case C: return 3;
	}
}

fn int all_constant(Foo e, Foo x)
{
	switch (e)
	{
		case x: return 0;
		case A: return 1;
		case B: return 2;
		case C: return 3;
	}
}
