module main;

faultdef F;

enum Foo : const fault
{
    F = F // #error: Recursive resolution of expression
}

fn void main() {}