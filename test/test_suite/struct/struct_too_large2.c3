import std;
struct Foo
{
    int a;
    int[0x4000_0000][0x4000_0000] b; // #error: Struct member 'b' would cause the struct to become too large
}

fn int main()
{
    Foo c = (Foo){};
    return 0;
}
