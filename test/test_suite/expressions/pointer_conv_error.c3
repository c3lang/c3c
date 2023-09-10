fn void test1()
{
    int myInt = 1;
    int* p1 = myInt;                    // #error: integer smaller
}

fn void test2()
{
    uint myUInt = 1;
    int* p2 = myUInt;                   // #error: integer smaller
}

fn void test3()
{
    uint myUInt = 1;
    int* p2 = (int*)(myUInt);       // #error: integer smaller
}

