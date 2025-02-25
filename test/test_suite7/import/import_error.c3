module test;
import std::core::mem;
import hello_world; // #error: No module named
import test; // #error: Importing the current

fn void hello() { }