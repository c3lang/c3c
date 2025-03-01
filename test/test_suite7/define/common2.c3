int ofek;
const OFKEOK = 2;
distinct Helo = int;
fn void Helo.test(&self) {}

def hupp = Helo.test;
def AOFKE = ofek; // #error: An uppercase alias is expected
def okfoe = OFKEOK; // #error: An alias starting with a lower
def helo = Helo; // #error: To alias a type
def HELO = Helo; // #error: To alias a type
