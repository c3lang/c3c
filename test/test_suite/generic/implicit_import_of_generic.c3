module test;
import std::io;

import abc;
alias Def2 = Def;
alias Test = abc::cde::Abc{int};

module abc;

fn void test() {}

module abc::foo;

struct Def { int x; }

module abc::cde <Type>;

struct Abc { Type a, b; }