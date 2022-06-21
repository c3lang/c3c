macro foo(a, $b, $Type) {}

macro @foo2(a, $b, $Type) {}

macro bar(&x) // #error: non-function
{}

macro baz(#y) {} // #error: non-function

macro baz2(a; @body()) {} // #error: non-function

