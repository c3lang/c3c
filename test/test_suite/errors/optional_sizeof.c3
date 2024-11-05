fn int! abc()
{
	return 1;
}
macro test()
{
	return @catch(abc())?!!;
}

macro test2()
{
	return @catch(abc())?;
}

fn void a()
{
	String s = $typeof(test()).qnameof; // #error: This expression lacks
}

fn void b()
{
	$sizeof(test()); // #error: This expression lacks
}

fn void c()
{
	$sizeof(test2() ?? 1);
}

fn void! d()
{
	$typeof(test2()!) g; // #error: This expression lacks a concrete type
}

macro e()
{
	var g = test2()!; // #error: No type can be inferred from the optional result
}

fn void! h()
{
	e();
}
