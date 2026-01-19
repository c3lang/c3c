module foo <Type, SIZE>;

struct Foo
{
  Type[SIZE] x;
}
module bar;
import foo;

alias Bar = Foo{int, int}; // #error: Expected a value, not a type
alias Baz = Foo{5, 4}; // #error: Expected a type, not a value
