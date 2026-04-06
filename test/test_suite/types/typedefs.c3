alias Arr = int[4];

Arr a = { 3, 4, 5, 6 };

fn void test1()
{
    Arr b = { 3, 4, 5, 6 };
    int c = b;              // #error: 'Arr' (int[4]) to 'int'
}

fn void test2()
{
    int d = a;              // #error: 'Arr' (int[4]) to 'int'
}

