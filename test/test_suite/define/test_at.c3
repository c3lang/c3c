module foo <Type>;
import std::io;

macro @hello(Type thing) {
    io::printfn("%d", $sizeof(thing));
}
fn void hello() {}

module bar;

import foo @public;

typedef Int = int;
macro Int.@test(self) {}

alias intHello = foo::@hello{int}; // #error: An at-macro like
alias @intHello = foo::hello{int};
alias @intTest = Int.@test;
alias intTest = Int.@test;           // #error: An at-macro like


fn void main(String[] args) {
    @intHello(42);
}