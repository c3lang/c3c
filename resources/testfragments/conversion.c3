module conversion;

func int test(int a0, uint b0, ushort c0, short d0)
{
    int a = 0;
    uint b = 0;
    ushort c = 0;
    short d = 0;
    a = a0;
    b = b0;
    c = c0;
    d = d0;
    b += 1;
    a += 2;
    // a += b; Error
    a += c;
    b += c;
    a += d;
    // b += d; Error
    b = b + b;
    a = c + c;
    a = c - c;
    a = c * c;
    a = c / c;
    a = c % c;
    a = c +% c;
    a = c -% c;
    a = c *% c;
    a = d + cast(c + c as ushort);
    // a = c << 2; TODO
    //a = c << c;
    //a <<= c;
    //a >>= c;
    //a = c >> c;
    // a = 2 << c; TODO
    return a;
}


func int test_top_down(int a0 as uint b0 as ushort c0, short d0)
{
    int a = 0;
    uint b = 0;
    ushort c = 0;
    short d = 0;
    a = d + cast(c +% c as ushort);
    return a;
}