module test;

union Xu
{
    void *b;
}

fn Xu foo()
{
    Xu a;
    a.b = (void*)(123);
    return a;
}