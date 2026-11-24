module x;
import a;
Aa{int} v;

module a <Ba>;
import b;
alias Cc = Aa{ Aa{ Bb{ Ba } } }; // #error: This generic instantiation would create more than 4096 instances
struct Aa { int x; }

module b <Ba>;
struct Bb { int x; }
