int ofek;
const OFKEOK = 2;
distinct Helo = int;
fn void Helo.test(&self) {}

alias hupp = Helo.test;
alias AOFKE = ofek; // #error: An uppercase alias is expected
alias okfoe = OFKEOK; // #error: An alias starting with a lower
alias helo = Helo; // #error: To alias a type
alias HELO = Helo; // #error: To alias a type
