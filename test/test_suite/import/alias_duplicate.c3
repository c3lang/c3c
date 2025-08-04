module abc;
import std;

alias xyz1 = module std::io;
alias xyz2 = module std::io;
alias xyz3 = module std::io;
alias xyz2 = module std::io; // #error: The module alias must be unique