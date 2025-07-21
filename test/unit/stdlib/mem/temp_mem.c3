module test;

fn String add(String s, Allocator a, int x)
{
    if (x < 0) return s.copy(a);
    String tmp;
    @pool()
    {
        tmp = "foo".tconcat(s);
        tmp = add(tmp, a, x - 1);
    };
    ulong* y = mem::talloc(ulong);
    *y = 0xAAAA_AAAA_AAAA_AAAA;
    return tmp.concat(a, "a");
}

fn String breakit(String s, Allocator a)
{
    @pool()
    {
        return inner2("foo".concat(tmem, s), a);
    };
}

fn String inner2(String s, Allocator a)
{
    @pool()
    {
        ulong* z1 = mem::talloc(ulong);
        *z1 = 0xAAAA_AAAA_AAAA_AAAA;
        String y = inner3(s, a);
        ulong* z = mem::talloc(ulong);
        *z = 0xAAAA_AAAA_AAAA_AAAA;
        return y;
    };
}

fn String inner3(String s, Allocator a)
{
    @pool()
    {
        ulong* z1 = mem::talloc(ulong);
        *z1 = 0xAAAA_AAAA_AAAA_AAAA;
        String y = inner4(s, a);
        ulong* z = mem::talloc(ulong);
        *z = 0xAAAA_AAAA_AAAA_AAAA;
        return y;
    };
}

fn String inner4(String s, Allocator a)
{
    @pool()
    {
        String y = s.concat(tmem, "xy**********").copy(a);
        return y;
    };
}

fn void test_temp_allocator() @test
{
    assert("foofoofoofoofoofooabcaaaaaa" == add("abc", tmem, 5), "was %s", add("abc", tmem, 5));
}

fn void test_temp_allocator2() @test
{
    assert("fooxyz0123456789xy**********" == breakit("xyz0123456789", tmem));
}
