module test;

union Xu
{
    void *b;
}

func Xu foo()
{
    Xu a;
    a.b = (void*)(123);
    return a;
}