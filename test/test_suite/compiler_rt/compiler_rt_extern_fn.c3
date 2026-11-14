module compiler_rt::banned;

extern fn int puts(char*); // #error: Extern functions are not allowed in compiler-rt

module test;
fn void main() {}
