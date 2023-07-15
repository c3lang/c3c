module testing;
import std::io;
import foo;

def Foo = foo::Foo;

fn void main()
{
    Foo f;
}

module foo;
import foo::private;

// Bug #856
def Foo = distinct inline PrivateFoo(<int>); // #error: could not be found

module foo::private(<Type>) @local;

struct PrivateFoo
{
	Type x;
}