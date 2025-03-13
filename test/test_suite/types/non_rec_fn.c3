
alias BlahFn = fn int(Foo f);
alias FooFn = fn int(Foo f, int j);
struct Foo
{
    BlahFn x;
    FooFn okf;
}


Foo g;