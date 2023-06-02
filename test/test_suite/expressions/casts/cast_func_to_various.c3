struct Struct
{
    int x;
}

enum Enum : uptr
{
    A, B
}

def Func = fn void(int);
def FuncOther = fn bool(char*);
def FuncSame = fn void(int);


fn void test1(Func arg)
{
    bool a = (bool)(arg);
    bool b = arg;                          // #error: 'Func' (fn void(int)) to 'bool'
}

fn void test2(Func arg)
{
    ichar b = (ichar)(arg);               // #error: 'Func' (fn void(int)) to 'ichar'
}

fn void test3(Func arg)
{
    uint c = (uint)(arg);                 // #error: 'Func' (fn void(int)) to 'uint'
}

fn void test4(Func arg)
{
    float d = (float)(arg);               // #error: 'Func' (fn void(int)) to 'float'
}

fn void test7(Func arg)
{
    usz g = (usz)(arg);
    FuncOther k = (FuncOther)(arg);
    FuncSame l = (FuncSame)(arg);
    FuncOther ke = arg;                         // #error: 'Func' (fn void(int)) to 'FuncOther' (fn bool(char*))
    FuncSame fe = arg;
    Enum j = (Enum)(arg);                 // #error: 'Func' (fn void(int)) to 'Enum'
}
