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

def Func = fn void(Enum);

fn void test1(Enum e)
{
    bool a = (bool)e; // #error: bool
    char b = (char)e.ordinal;
    uint c = (uint)e.ordinal;
    float d = (float)e; // #error: float
    uint* f = (uint*)e;         // #error: 'uint*'
}

fn void test2(Enum e)
{
    Struct* g = (Struct*)(e);     // #error: 'Struct*'
}

fn void test3(Enum e)
{
    EnumB h = (EnumB)(e);         // #error: 'EnumB'
    Func i = (Func)(e);           // #error: 'Func'
}

