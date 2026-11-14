module compiler_rt::banned;

interface Foo { fn void x(); } // #error: Interfaces may not be used in compiler-rt

module test;
fn void main() {}
