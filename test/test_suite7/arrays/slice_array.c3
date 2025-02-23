distinct Bar = inline void;

struct Foo
{
    Bar[] bars; // #error: You cannot form a slice with elements
}