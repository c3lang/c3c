module compiler_rt::a::b;

tlocal int tl; // #error: Thread locals may not be used in compiler-rt

module test;
fn void main() {}
