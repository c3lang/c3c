module withbody;


extern fn int printf(char *, ...);

struct Foo
{
    int x;
}

fn int Foo.mutate(Foo *foo)
{
    printf("Mutating\n");
    return 10 * ++foo.x;
}

macro @macro_with_body(foo, #x; @body(x, y))
{
    #x = foo.x;
    @body(foo.mutate(), &#x); // #error: 'int*' to 'int'
}


fn void main()
{
    Foo f = { 33 };
    int y;
    @macro_with_body(f, y; int x, int dy)
    {
        printf("Got values %d, %d\n", x, dy);
    };

}