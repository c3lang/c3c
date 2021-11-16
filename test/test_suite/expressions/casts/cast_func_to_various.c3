struct Struct
{
    int x;
}

enum Enum : uptr
{
    A, B
}

define Func = fn void(int);
define FuncOther = fn bool(char*);
define FuncSame = fn void(int);


fn void test1(Func arg)
{
    bool a = (bool)(arg);
    bool b = arg;
}

fn void test2(Func arg)
{
    ichar b = (ichar)(arg);               // #error: 'Func' (func void(int)) to 'ichar'
}

fn void test3(Func arg)
{
    uint c = (uint)(arg);                 // #error: 'Func' (func void(int)) to 'uint'
}

fn void test4(Func arg)
{
    float d = (float)(arg);               // #error: 'Func' (func void(int)) to 'float'
}

fn void test7(Func arg)
{
    usize g = (usize)(arg);
    FuncOther k = (FuncOther)(arg);
    FuncSame l = (FuncSame)(arg);
    FuncOther ke = arg;                         // #error: 'Func' (func void(int)) to 'FuncOther' (func bool(char*))
    FuncSame fe = arg;
    Enum j = (Enum)(arg);                 // #error: 'Func' (func void(int)) to 'Enum'
}
