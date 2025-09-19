// #deprecation: no
module test;
import std::collections::maybe;

fn void main()
{
    Maybe{float} f = (Maybe) {.value=6.6, .has_value=true};
    Maybe{int}g = (Maybe) {.value=8, .has_value=true};
}
