struct Struct
{
    int x;
}

enum Enum : uint
{
    A, B
}

typedef func void(int) as Func;

typedef func bool(char*) as FuncOther;

typedef func void(int) as FuncSame;


func void test1(Func arg)
{
    bool a = cast(arg as bool);
}

func void test2(Func arg)
{
    ichar b = cast(arg as ichar);
}

func void test3(Func arg)
{
    uint c = cast(arg as uint);
}

func void test4(Func arg)
{
    float d = cast(arg as float);               // #error: Cannot cast 'Func' (func void()) to 'float'.
}

func void test7(Func arg)
{
    usize g = cast(arg as usize);
    FuncOther k = cast(arg as FuncOther);
    FuncSame l = cast(arg as FuncSame);
    FuncOther ke = arg;                         // #error: Cannot implicitly cast 'Func' (func void()) to 'FuncOther' ('func bool()')
    FuncSame fe = arg;
    Enum j = cast(arg as Enum);                 // #error: Cannot cast 'Func' (func void()) to 'Enum'.
}
