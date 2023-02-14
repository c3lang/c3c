import std::io;

define foo = distinct x; // #error: can only be used with types
define bar = distinct int; // #error: type name alias must start with an uppercase
