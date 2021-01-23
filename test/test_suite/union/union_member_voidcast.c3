module test;

union Xu
{
    void *b;
}

func Xu foo()
{
    Xu a;
    a.b = cast(123 as void*);
    return a;
}