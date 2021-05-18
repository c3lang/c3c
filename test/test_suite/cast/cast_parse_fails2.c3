
struct Foo { int a; }

func void test1()
{
    Foo({ 32 });
    (Foo)({ 32 }); // #error: Unexpected start of a block '{' here. If you intended a compound literal
}