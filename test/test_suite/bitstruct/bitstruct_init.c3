// #skip
module foo;

bitstruct Foo : uint
{
    int x : 1..3;
    uint y : 11..13;
    int z : 15..15;
}

struct Bar
{
    int x;
    bitstruct baz : int
    {
        int x : 1..3;
    }
}

struct Bar2
{
    int x;
    bitstruct : int
    {
        int z : 1..3;
    }

}
fn void testNested()
{
    Bar b1 = { 3, { 3 } };
    Bar b2 = { .x = 3, .baz.x = 3 };
    Bar2 b3 = { 1, 3 };
    Bar2 b4 = { .x = 123, .z = 3 };
    Bar2 b5 = { .x = 123, .z = 4 };  // #error: would be truncated
    Bar2 b6 = { 1, 3 }; // #error: would be truncated
    Bar b7 = { 3, { 4 } }; // #error: would be truncated
    Bar b8 = { .x = 3, .baz.x = 4 }; // #error: would be truncated

}

fn void test()
{
    Foo abc = {};
    abc.x = -4;
    abc.z = 0;
    abc.z = -1;
    abc.x = 3;
    abc.y = 7;
    abc.x = -5; // #error: would be truncated
    abc.x = 4; // #error: would be truncated
    abc.y = 8; // #error: would be truncated
}

fn void test2()
{
    Foo abc = { -4, 8, 0 }; // #error: would be truncated
}

fn void test3()
{
    Foo abc = { .x = -4, .z = 0, .y = 8 }; // #error: would be truncated
}