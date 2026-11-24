alias Aa = Aa; // #error: Recursive definition of 'Aa'
fn int Aa.x() => 0;

alias Bb = Cc; // #error: Recursive definition of 'Bb'
alias Cc = Bb;
fn int Bb.x() => 0;

alias Dd = Ee;
alias Ee = Ff; // #error: Recursive definition of 'Ee'
alias Ff = Ee;
fn int Dd.x() => 0;

alias Gg = int;
alias Hh = Gg;
fn int Hh.y(self) => 1;
