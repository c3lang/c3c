module foo;

fn void foo()
{
	int? x;
	if (catch x)
	{
		z = 1233; // #error: could not be found, did you spell it right
		return;
	}
	int y = x;
}

fn void main()
{
	int? x;
	if (catch err = BAR) // #error: 'BAR' could not be found
	{
		return;
	}
	int y = x;
}