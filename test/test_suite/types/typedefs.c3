typedef int[4] as Arr;

Arr a = { 3, 4, 5, 6 };

func void test1()
{
    Arr b = { 3, 4, 5, 6 };
    int c = b;              // #error: cast 'Arr' (int[4]) to 'int'
    int d = a;              // #error: cast 'Arr' (int[4]) to 'int'
}

typedef Number1 as Number2; // #error: Recursive definition of 'Number2'
typedef Number2 as Number1;

typedef Number as Number; // #error: Recursive definition of 'Number'


