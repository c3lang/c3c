module foo<Type>;
import std::io;

macro @hello(Type thing) {
    io::printfln("%d", $sizeof(thing));
}

module bar;

import private foo;

define intHello = foo::@hello<int>; // #error: cannot be aliased
define @intHello = foo::hello<int>; // #error: cannot use

fn void main(String[] args) {
    @intHello(42);
}