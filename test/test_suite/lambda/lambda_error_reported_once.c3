alias Fp = fn int();

macro m()
{
	Fp f = fn() => foo; // #error: 'foo' could not be found
	return f;
}

fn void repeated_macro()
{
	m();
	m();
	m();
}

macro int @twice($v)
{
	Fp f = fn() => $v;
	return f() * 2;
}

fn int valid()
{
	return @twice(1) + @twice(2) + @twice(1);
}
