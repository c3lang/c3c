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

fn void test_ternary_temp_lifetime()
{
    bool c = true;
    int* p = c ? &&5 : &&7;
    int* p2 = c ? &&999 : &&888;
    assert(*p == 5);
    assert(*p2 == 999);
}

fn void test_ternary_slice_temp_lifetime()
{
    bool c = true;
    int[] s1 = c ? (int[]){ 10, 20 } : (int[]){ 30, 40 };
    int[] s2 = c ? (int[]){ 100, 200 } : (int[]){ 300, 400 };
    assert(s1[0] == 10 && s1[1] == 20);
    assert(s2[0] == 100 && s2[1] == 200);
}

fn void test_ternary_any_lifetime()
{
    bool c = true;
    any a = c ? &&5 : &&7;
    assert(*(int*)a.ptr == 5);
}

