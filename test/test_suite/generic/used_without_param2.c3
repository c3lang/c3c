// #deprecation: no
module test;
import std::collections::maybe;

fn void test()
{
    maybe::Maybe{float} f = (Maybe) {.value=6.6, .has_value=true}; // #error: but no parameters where given
    Maybe x; // #error: but no parameters where given
}

