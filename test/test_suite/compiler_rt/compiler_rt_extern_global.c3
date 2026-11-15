module compiler_rt::banned;

extern int some_global; // #error: Extern globals are not allowed in compiler-rt

module test;
fn void main() {}
