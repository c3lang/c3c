module main;

excuse F;

constset Foo : fault
{
    F = F // #error: Recursive resolution of expression
}

fn void main() {}