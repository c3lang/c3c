fn void test(double d, bool b, String s)
{
	switch (d)
	{
		case true ? 1.0 : 0.0: break;
		case 1.0: break;
		case true ? 1.0 : 0.0: break;
		default: break;
	}
	switch (b)
	{
		case true: break;
		case !false: break;
		default: break;
	}
	switch (b)
	{
		case !false: break;
		case true: break;
		case !false: break;
		default: break;
	}
	switch (s)
	{
		case true ? "a" : "b": break;
		case "a": break;
		case true ? "a" : "b": break;
		default: break;
	}
}

fn void test_integer(int i)
{
	switch (i)
	{
		case true ? 1 : 0: break;
		case 1: break; // #error: The same case value appears more than once
		default: break;
	}
}
