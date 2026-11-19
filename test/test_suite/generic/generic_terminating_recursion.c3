module x;
import a, c;
Aa{int, int*} v;
Cc{int, 2} w;
fn void main() {}

module a <Ta, Tb>;
struct Aa { Ta a; Tb b; }
alias Yy = Aa{Tb, Tb};

module c <Tt, NN>;
struct Cc { Tt t; }
alias Up @if(NN > 0) = Cc{Tt*, NN - 1};
