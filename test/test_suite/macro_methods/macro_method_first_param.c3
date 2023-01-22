
struct Foo { int y; }

macro Foo.text(...) => f.y; // #error: The first parameter to this method must