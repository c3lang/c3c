fn void abc()
{
	if (var foo) {} // #error: must always have an
}

fn void abc2()
{
	for (var foo;;) {} // #error: must always have an
}


