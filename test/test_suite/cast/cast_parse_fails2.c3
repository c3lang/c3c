
struct Foo { int a; }

fn void test1()
{
    Foo({ 32 }); // #error: A type cannot be followed by (), if you intended a cast, use (type)(expression)
    (Foo)({ 32 }); // #error:  Casting from an untyped list to a concrete type is not possible.
}