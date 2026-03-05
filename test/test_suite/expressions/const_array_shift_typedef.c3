typedef Foo = int;

Foo[<2>] f3 = (Foo[<2>]) 1 << 2; // #error: The expression must be a constant value

fn int main() => 0;