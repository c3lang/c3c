module main;

faultdef F;

const enum Foo : fault
{
    F = F // #error: Recursive resolution of expression
}

fn void main() {}