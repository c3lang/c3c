struct Foo @tag("", Foo.tagof("")) // #error: Recursive definition of 'Foo'.
{
}
fn int main() => 1;