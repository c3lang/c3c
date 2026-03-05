module foo::a;
struct Lexer { int a; }
fn void foo(Lexer *lex) {}

module foo::b;
struct Lexer { int a; }
fn void foo(Lexer *lex) {}

module foo;

fn void bar()
{
  a::Lexer l;
  b::foo(&l); // #error: Implicitly casting 'foo::a::Lexer*' to 'foo::b::Lexer*' is not permitted, but you may do an explicit cast by placing '(foo::b::Lexer*)' before the expression
}
