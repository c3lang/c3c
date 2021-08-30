func void test1()
{
    int myInt = 1;
    int* p1 = myInt;                    // #error: 'int' into 'int*'
}

func void test2()
{
    uint myUInt = 1;
    int* p2 = myUInt;                   // #error: 'uint' into 'int*'
}

func void test3()
{
    uint myUInt = 1;
    int* p2 = (int*)(myUInt);     // #error: 'uint' to 'int*'
}

