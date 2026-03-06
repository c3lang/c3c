module main;

faultdef F;

constdef Foo : fault
{
    F = F // #error: Recursive resolution of expression
}

fn void main() {}