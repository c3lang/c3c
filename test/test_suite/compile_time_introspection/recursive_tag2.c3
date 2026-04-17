struct Foo @tag("", Foo::get_tag("")) // #error: Recursive definition of 'Foo'.
{
}
fn int main() => 1;