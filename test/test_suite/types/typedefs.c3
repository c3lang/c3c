define Arr = int[4];

Arr a = { 3, 4, 5, 6 };

func void test1()
{
    Arr b = { 3, 4, 5, 6 };
    int c = b;              // #error: cast 'Arr' (int[4]) to 'int'
    int d = a;              // #error: cast 'Arr' (int[4]) to 'int'
}

