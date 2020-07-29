


struct Struct
{
    int x;
}

enum Enum : uint
{
    A, B
}

enum EnumB : byte
{
    C, D
}

typedef func void(Enum) as Func;

func void test1(Enum e)
{
    bool a = cast(e as bool);
    byte b = cast(e as byte);
    uint c = cast(e as uint);
    float d = cast(e as float);
    uint* f = cast(e as uint*);
}

func void test2(Enum e)
{
    Struct* g = cast(e as Struct*);
}

func void test3(Enum e)
{
    //EnumB h = cast(e as EnumB);
    //Func i = cast(e as Func);
}
