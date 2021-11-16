module test;
import std::mem;
import std::mem; // #error: was imported more
import hello_world; // #error: No module named
import test; // #error: Importing the current

fn void hello() { }