import std;
enum Foo
{
}
EnumMap{Foo, int} map; // #error: Parameter(s) would violate constraint: @require "Enum.values.len > 0" violated
fn int main() => 1;