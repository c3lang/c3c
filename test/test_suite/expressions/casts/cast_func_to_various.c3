struct Struct
{
    int x;
}

enum Enum : uptr
{
    A, B
}

typedef func void(int) as Func;

typedef func bool(char*) as FuncOther;

typedef func void(int) as FuncSame;


func void test1(Func arg)
{
    bool a = (bool)(arg);
    bool b = arg;
}

func void test2(Func arg)
{
    ichar b = (ichar)(arg);               // #error: Cannot cast 'Func' (func void(int)) to 'ichar'.
}

func void test3(Func arg)
{
    uint c = (uint)(arg);                 // #error: Cannot cast 'Func' (func void(int)) to 'uint'.
}

func void test4(Func arg)
{
    float d = (float)(arg);               // #error: Cannot cast 'Func' (func void(int)) to 'float'.
}

func void test7(Func arg)
{
    usize g = (usize)(arg);
    FuncOther k = (FuncOther)(arg);
    FuncSame l = (FuncSame)(arg);
    FuncOther ke = arg;                         // #error: Cannot implicitly cast 'Func' (func void(int)) to 'FuncOther' (func bool(char*))
    FuncSame fe = arg;
    Enum j = (Enum)(arg);                 // #error: Cannot cast 'Func' (func void(int)) to 'Enum'.
}
