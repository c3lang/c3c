module test;
import std::collections::maybe;

fn void test()
{
    maybe::Maybe(<float>) f = Maybe {.value=6.6, .has_value=true}; // #error: Did you mean the struct
    Maybe x; // #error: Did you mean the struct
}

