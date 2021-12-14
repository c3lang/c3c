define Foo = int[0];

struct Bar
{
    Foo x; // #error: Zero length arrays are not valid members.
    int b;
}