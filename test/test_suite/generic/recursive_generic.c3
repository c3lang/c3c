module test;
import test1;

fn int main() => 0;

Aa {int} a;

module test1 {Type};
import test2;
alias Cc = Aa {Bb {Type}}; // #error:in the same module as the original is not allowed
struct Aa {Type a;}

module test2 {Type};
struct Bb {Type b;}