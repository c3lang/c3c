// #opt: --compiler-rt-module
// #opt: my::rt
module compiler_rt::not_special;

tlocal int allowed_here;
fn void init() @init {}

module my::rt::sub;

tlocal int tl; // #error: Thread locals may not be used in compiler-rt

module test;
fn void main() {}
