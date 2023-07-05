module foo(<Type>);
import std::io;

macro @hello(Type thing) {
    io::printfn("%d", $sizeof(thing));
}

module bar;

import foo @public;

def intHello = foo::@hello(<int>); // #error: cannot be aliased
def @intHello = foo::hello(<int>); // #error: cannot use

fn void main(String[] args) {
    @intHello(42);
}