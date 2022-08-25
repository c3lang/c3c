macro foo(a, $b, $Type) {}

macro @foo2(a, $b, $Type) {}

macro bar(&x) // #error: Ref and expression parameters
{}

macro baz(#y) {} // #error: Ref and expression parameters

macro baz2(a; @body()) {} // #error: Names of macros

