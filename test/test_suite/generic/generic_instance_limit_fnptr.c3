module x;
import a;
Aa{int} v;

module a <Tt>;
struct Aa { Tt t; }
alias Ff = fn void(Tt);
alias Gg = fn int(Tt);
alias Xx = Aa{Ff}; // #error: This generic instantiation would create more than 4096 instances
alias Yy = Aa{Gg};
