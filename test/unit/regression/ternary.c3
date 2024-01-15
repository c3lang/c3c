module test @test;

fn void const_ternary()
{
	int foo = 1;
    const int FOO = 1;
    assert((foo ?: 2) == 1);
    assert((FOO ?: 2) == 1);
    int bar = 2;
    assert((FOO ?: bar) == 1);
}
