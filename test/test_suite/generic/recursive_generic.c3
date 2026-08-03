module test;
import test1;

fn int main() => 0;

Aa {int} a;

module test1 <Type>;
import test2;
alias Cc = Aa {Bb {Type}}; // #error:  The generic declaration is too deeply nested and is possibly recursive
struct Aa {Type a;}

module test2 <Type>;
struct Bb {Type b;}