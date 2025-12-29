module test;
import std::collections::map;

fn void foo(HashMap{char[], String}* f) { }

fn void main()
{
	HashMap{char[], char[]} x;
	foo(&x); // #error: Implicitly casting 'HashMap{char[], char[]}*' to 'HashMap{char[], String}*' is not permitted, but you may do an explicit cast by placing '(HashMap{char[], String}*)' before the expression
}
