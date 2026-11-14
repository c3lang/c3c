module compiler_rt::banned;

fn void fin() @finalizer {} // #error: Functions with '@init' and '@finalizer' may not be used in compiler-rt

module test;
fn void main() {}
