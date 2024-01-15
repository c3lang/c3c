module test;

fn String add(String s, Allocator* a, int x)
{
    if (x < 0) return s.copy(a);
    String tmp;
    @pool(a)
    {
        tmp = "foo".tconcat(s);
        tmp = add(tmp, a, x - 1);
    };
    ulong* y = mem::temp_alloc(ulong);
    *y = 0xAAAA_AAAA_AAAA_AAAA;
    return tmp.concat("a", .allocator = a);
}

fn String breakit(String s, Allocator* a)
{
    @pool(a)
    {
        return inner2("foo".concat(s, allocator::temp()), a);
    };
}

fn String inner2(String s, Allocator* a)
{
    @pool(a)
    {
        ulong* z1 = mem::temp_alloc(ulong);
        *z1 = 0xAAAA_AAAA_AAAA_AAAA;
        String y = inner3(s, a);
        ulong* z = mem::temp_alloc(ulong);
        *z = 0xAAAA_AAAA_AAAA_AAAA;
        return y;
    };
}

fn String inner3(String s, Allocator* a)
{
    @pool(a)
    {
        ulong* z1 = mem::temp_alloc(ulong);
        *z1 = 0xAAAA_AAAA_AAAA_AAAA;
        String y = inner4(s, a);
        ulong* z = mem::temp_alloc(ulong);
        *z = 0xAAAA_AAAA_AAAA_AAAA;
        return y;
    };
}

fn String inner4(String s, Allocator* a)
{
    @pool(a)
    {
        String y = s.concat("xy**********", allocator::temp()).copy(a);
        return y;
    };
}

fn void! test_temp_allocator() @test
{
    assert("foofoofoofoofoofooabcaaaaaa" == add("abc", allocator::temp(), 5), "was %s", add("abc", allocator::temp(), 5));
}

fn void! test_temp_allocator2() @test
{
    assert("fooxyz0123456789xy**********" == breakit("xyz0123456789", allocator::temp()));
}
