// #deprecation: no
module test;
import std::collections::maybe;

fn void test()
{
    maybe::Maybe{float} f = (Maybe) {.value=6.6, .has_value=true};
    Maybe x; // #error: 'Maybe' is defined in the generic module 'std::collections::maybe', did you forget the parameters '{ ... }'
}

