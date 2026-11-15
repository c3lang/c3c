module test;

fn void strings(String x)
{
	switch (x)
	{
		case "a":
			return;
		default:
		case "b": // #error: its cases are evaluated in order
			return;
	}
}

fn void types(typeid t)
{
	switch (t)
	{
		default:
			return;
		case int: // #error: its cases are evaluated in order
			return;
	}
}

fn void runtime_case(int x, int y)
{
	switch (x)
	{
		case y:
			return;
		default:
			return;
		case 2: // #error: its cases are evaluated in order
			return;
	}
}

fn void ints_are_fine(int x)
{
	switch (x)
	{
		default:
		case 1:
			return;
	}
}

fn void default_last_is_fine(String x)
{
	switch (x)
	{
		case "a":
		default:
			return;
	}
}
