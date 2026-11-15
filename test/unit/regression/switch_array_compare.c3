module switch_array_compare;

fn void switch_on_float_array() @test
{
	double[2] d = { -0.0, 1.0 };
	switch (d)
	{
		case { 0.0, 1.0 }: break;
		default: unreachable();
	}
}

fn void switch_on_string_array() @test
{
	char[2] buf = { 'a', 'b' };
	String[2] s = { (String)buf[..], "c" };
	switch (s)
	{
		case { "ab", "c" }: break;
		default: unreachable();
	}
}
