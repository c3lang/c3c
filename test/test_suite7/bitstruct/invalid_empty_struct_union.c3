struct Foo // #error: Zero sized structs are not permitted.
{
}

union Bar // #error: Zero sized unions are not permitted.
{
}