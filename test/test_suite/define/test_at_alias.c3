module foo <Type>;
import std::io;

macro @hello(Type thing) {
    io::printfn("%d", $sizeof(thing));
}

module bar;

import foo @public;

alias @intHello = foo::@hello{int};

fn void main(String[] args) {
    @intHello(42);
}