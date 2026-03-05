// #deprecation: no
module test;
import std::collections::maybe;

fn void test()
{
    (Maybe) {.value=6.6, .has_value=true}; // #error: 'Maybe' is a generic struct, did you forget the parameters '{ ... }'
}