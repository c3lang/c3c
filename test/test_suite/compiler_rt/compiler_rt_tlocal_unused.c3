module compiler_rt::banned;

tlocal int tl @local; // #error: Thread locals may not be used in compiler-rt

module test;
fn void main() {}
