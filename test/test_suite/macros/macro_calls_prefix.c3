macro foo(a, $b, $Type) {}

macro @foo2(a, $b, $Type) {}

macro baz(#y) {} // #error: are not allowed in function-like

macro baz2(a; @body()) {} // #error: Names of macros

