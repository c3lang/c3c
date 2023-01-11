fn void test1()
{
	while (1) defer foo(); // #error: Looping over a raw 'defer'
}

fn void test1a()
{
	while (1) { defer foo(); }
}

fn void test2()
{
	if (1) defer foo(); // #error: An 'if' statement may not be followed by a raw 'defer'
}

fn void test2a()
{
	if (1) { defer foo(); }
}

fn void test3()
{
	defer defer foo(); // #error: A defer may not have a body consisting of a raw 'defer'
}

fn void test3a()
{
	defer { defer foo(); }
}

fn void test4()
{
	for (;;) defer foo(); // #error: Looping over a raw 'defer'
}

fn void test4a()
{
	for(;;) { defer foo(); }
}

fn void foo() {}
