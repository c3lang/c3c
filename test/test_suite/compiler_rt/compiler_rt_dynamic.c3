module compiler_rt::banned;

struct Bar { int x; }
fn char* Bar.name(&self) @dynamic { return null; } // #error: '@dynamic' methods may not be used in compiler-rt

module test;
fn void main() {}
