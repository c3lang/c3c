define Arr = int[4];

Arr a = { 3, 4, 5, 6 };

func void test1()
{
    Arr b = { 3, 4, 5, 6 };
    int c = b;              // #error: 'Arr' (int[4]) into 'int'
    int d = a;              // #error: 'Arr' (int[4]) into 'int'
}

