fn int! abc()
{
	return 1;
}
macro test()
{
	abc()!;
}

fn void a()
{
	String s = $typeof(test()).qnameof; // #error: This optional expression is untyped.
}

fn void b()
{
	$sizeof(test()); // #error: This optional expression is untyped.
}

fn void c()
{
	$sizeof(test() ?? 1);
}

fn void! d()
{
	$typeof(test()!) g; // #error: This expression has no concrete type
}

macro e()
{
	var g = test()!; // #error: No type can be inferred from the optional result
}

fn void! h()
{
	e();
}