module compiler_rt::banned;

fn void init() @init {} // #error: Functions with '@init' and '@finalizer' may not be used in compiler-rt

module test;
fn void main() {}
