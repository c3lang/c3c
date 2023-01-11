struct Foo
{
    int x;
    int[*] y; // #error: flexible array member must be the last element
    int z;
}

struct Bar
{
    int[*] y; // #error: flexible array member cannot be the only element
}

struct Baz
{
    int y;
    int[*] z;
}

struct BazContainerOk
{
    int z;
    Baz c;
}

struct BazContainer
{
    Baz c; // #error: A struct member with a flexible array must be the last element.
    int y;
}

Baz[5] ab; // #error: Arrays of structs with flexible array members is not allowed.