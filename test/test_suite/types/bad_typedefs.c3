module test;
import std;

struct Foo { struct x { int a; } }
alias Floo = $typeof(Foo.x); // #error: You cannot create an alias for a member reference as it is a compile time type
alias Void = void?;  // #error: You cannot create an alias for an optional type
typedef Floo2 = $typeof(Foo.x); // #error: You cannot create a distinct type for a member reference
typedef Void2 = void?; // #error: You cannot create a distinct type from

fn void main()
{

}