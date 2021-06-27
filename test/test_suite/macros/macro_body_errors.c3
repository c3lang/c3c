
macro foo1(;) { } // #error: Expected a trailing block with the format '@block(...)

macro foo2(;@body()) {}

macro foo3(;@body) {}

