module test;
import std::collections::maybe;

fn void main()
{
    Maybe(<float>) f = (Maybe) {.value=6.6, .has_value=true}; // #error: Did you mean the struct
    Maybe(<int>)g = (Maybe) {.value=8, .has_value=true};
}
