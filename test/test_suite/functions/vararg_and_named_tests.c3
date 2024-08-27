// #target: macos-x64
module foo;

extern fn void printf(char* format, ...);

fn void test(int x, int... y, int z = 2)
{
}

fn void test2(int x, int... y, int z)
{
}

fn void a()
{
	test(z: 32, 3); // #error: Named arguments must be placed after positional arguments
}

fn void b()
{
	test(1, x: 3); // #error: The parameter 'x' was already set.
}

fn void c()
{
	test(1);
}

fn void d()
{
	test2(1, z: 3);
	test2(1, 2, 3); // #error: Expected 'z: ...' after this argument
}


fn void single(int x) {}

fn void e()
{
	single(); // #error: This call expected a parameter of type 'int'
}

fn void multiple(int x, int y) {}

fn void f()
{
	multiple(1); // #error: Expected 1 more argument after this one
}

fn void g()
{
	multiple(); // #error: 'multiple' expects
}