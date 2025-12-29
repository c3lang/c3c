import std;
struct Foo
{
    int a;
    int[int.max] b; // #error: Struct member 'b' would cause the struct to become too large
}

fn int main()
{
    Foo c = (Foo){};
    return 0;
}
