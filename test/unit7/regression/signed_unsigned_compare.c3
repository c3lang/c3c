module signed_unsigned_comparisons @test;

fn void test_signed()
{
    int a = 0;
    int b = 1;

    bool ab_gt = a > b;
    bool ab_ge = a >= b;
    bool ab_le = a <= b;
    bool ab_lt = a < b;
    bool ab_ne = a != b;
    bool ab_eq = a == b;

}

fn void test_unsigned()
{
    uint a = 0;
    uint b = 1;

    bool ab_gt = a > b;
    bool ab_ge = a >= b;
    bool ab_le = a <= b;
    bool ab_lt = a < b;
    bool ab_ne = a != b;
    bool ab_eq = a == b;

}

fn void test_signedunsigned()
{
    ichar a = 0 - 1;
    char b = (char)(a);

    assert(!(a > b));
    assert(!(a >= b));
    assert(a < b);
    assert(a <= b);
    assert(a != b);
    assert(!(a == b));

    a = 1;
    b = 1;

    assert(!(a > b));
    assert(a >= b);
    assert(!(a < b));
    assert(a <= b);
    assert(!(a != b));
    assert(a == b);

    a = 1;
    b = 4;

    assert(!(a > b));
    assert(!(a >= b));
    assert(a < b);
    assert(a <= b);
    assert(a != b);
    assert(!(a == b));

    a = 4;
    b = 1;

    assert(a > b);
    assert(a >= b);
    assert(!(a < b));
    assert(!(a <= b));
    assert(a != b);
    assert(!(a == b));

    a = 4;
    b = 129;

    assert(!(a > b));
    assert(!(a >= b));
    assert(a < b);
    assert(a <= b);
    assert(a != b);
    assert(!(a == b));

    a = 0 - 4;
    b = 129;

    assert(!(a > b));
    assert(!(a >= b));
    assert(a < b);
    assert(a <= b);
    assert(a != b);
    assert(!(a == b));

}

fn void test_unsignedsigned()
{
    int b = -1;
    uint a = (uint)(b);

    assert(a > b);
    assert(a >= b);
    assert(!(a < b));
    assert(!(a <= b));
    assert(a != b);
    assert(!(a == b));

    a = 1;
    b = 1;

    assert(!(a > b));
    assert(a >= b);
    assert(!(a < b));
    assert(a <= b);
    assert(!(a != b));
    assert(a == b);

    a = 4;
    b = 1;

    assert(a > b);
    assert(a >= b);
    assert(!(a < b));
    assert(!(a <= b));
    assert(a != b);
    assert(!(a == b));

    a = 1;
    b = 4;

    assert(!(a > b));
    assert(!(a >= b));
    assert(a < b);
    assert(a <= b);
    assert(a != b);
    assert(!(a == b));

    a = 0x8000_0001;
    b = 4;

    assert(a > b);
    assert(a >= b);
    assert(!(a < b));
    assert(!(a <= b));
    assert(a != b);
    assert(!(a == b));

    b = 0 - 4;
    a = 0x8000_0001;

    assert(a > b);
    assert(a >= b);
    assert(!(a < b));
    assert(!(a <= b));
    assert(a != b);
    assert(!(a == b));

}
