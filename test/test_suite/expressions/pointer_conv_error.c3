func void test1()
{
    int myInt = 1;
    int* p1 = myInt;                    // #error: Cannot implicitly cast 'int' to 'int*'.
}

func void test2()
{
    uint myUInt = 1;
    int* p2 = myUInt;                   // #error: Cannot implicitly cast 'uint' to 'int*'.
}

func void test3()
{
    uint myUInt = 1;
    int* p2 = cast(myUInt, int*);       // #error: Cannot cast to 'int*' from the smaller integer type 'uint'
}

