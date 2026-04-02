module test;
struct Foo
{
    int x;
}
fn void Foo.print_it(...) // #error: The first parameter of a method must always be a regular argument, it cannot be a vaarg
{
}