fn void foo1(int... x;) {} // #error: Expected ')'
fn void foo2(... x;) {} // #error: Expected ')'
fn void foo3(... x;) {} // #error: Expected ')'
fn void foo4(;) {} // #error: Expected ')'
fn void foo5(int x;) {} // #error: Expected ')'
fn void foo6(string[] x) {} // #error: Expected typename here. (Following '[' usually follows a type in a parameter list)