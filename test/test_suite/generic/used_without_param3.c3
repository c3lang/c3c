// #deprecation: no
module test;
import std::collections::maybe;

fn void test()
{
    (Maybe) {.value=6.6, .has_value=true}; // #error: is defined in the generic module
}