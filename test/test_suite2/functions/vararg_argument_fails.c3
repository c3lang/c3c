fn void foo5(..., ...) {} // #error: Only a single vararg parameter is allowed

fn void foo6(int ..., int ...) {} // #error: Only a single vararg parameter is allowed

fn void foo7(int... x, int ... y) {} // #error: Only a single vararg parameter is allowed