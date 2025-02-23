module foo(<Type>);
import std::io;

macro @hello(Type thing) {
    io::printfn("%d", $sizeof(thing));
}
fn void hello() {}

module bar;

import foo @public;

distinct Int = int;
macro Int.@test(self) {}

def intHello = foo::@hello(<int>); // #error: An at-macro like
def @intHello = foo::hello(<int>);
def @intTest = Int.@test;
def intTest = Int.@test;           // #error: An at-macro like


fn void main(String[] args) {
    @intHello(42);
}