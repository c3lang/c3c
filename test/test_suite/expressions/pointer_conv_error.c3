fn void test1()
{
    int myInt = 1;
    int* p1 = myInt;                    // #error: You cannot implicitly convert an integer to a pointer, you may use an explicit cast if you first widen the value to pointer size, e.g. '(int*)(uptr)some_value'
}

fn void test2()
{
    uint myUInt = 1;
    int* p2 = myUInt;                   // #error:  You cannot implicitly convert an integer to a pointer, you may use an explicit cast if you first widen the value to pointer size, e.g. '(int*)(uptr)some_value'
}

fn void test3()
{
    uint myUInt = 1;
    int* p2 = (int*)(myUInt);       // #error: integer smaller
}

