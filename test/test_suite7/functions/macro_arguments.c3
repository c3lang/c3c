
fn void foo1(int #foo) { } // #error: Only regular parameters are allowed for functions.

fn void foo2(int $foo) { } // #error: Only regular parameters are allowed for functions.

fn void foo3(bar) { } // #error: Only typed parameters are allowed for functions

fn void foo4($Type) { } // #error: Only regular parameters are allowed for functions.

fn void foo9(int x, int x) {} // #error: Duplicate parameter name 'x'.

macro @foo($a, $b, $c, ...) {}