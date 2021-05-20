


struct Struct
{
    int x;
}

enum Enum : uint
{
    A, B
}

enum EnumB : char
{
    C, D
}

define Func = func void(Enum);

func void test1(Enum e)
{
    bool a = (bool)(e);
    char b = (char)(e);
    uint c = (uint)(e);
    float d = (float)(e);
    uint* f = (uint*)(e);         // #error: cast 'Enum' to 'uint*'
}

func void test2(Enum e)
{
    Struct* g = (Struct*)(e);     // #error: cast 'Enum' to 'Struct*'
}

func void test3(Enum e)
{
    EnumB h = (EnumB)(e);
    Func i = (Func)(e);           // #error: cast 'Enum' to 'Func'
}
