
macro foo1(;) { } // #error: Expected an ending ')' or a block parameter on the

macro foo2(;@body()) {}

macro foo3(;@body) {}

