module compiler_rtx;

tlocal int tl;
extern fn int puts(char*);
interface Foo { fn void x(); }
fn void init() @init {}

module compiler_rt_other::sub;

tlocal int tl2;

module test;
fn void main() {}
