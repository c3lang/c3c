module glist @test;
import std::collections;

fn void simple_use()
{
	AnyList l;
	l.push(1);
	l.push("Hello");
	assert(l.get(1, String)!! == "Hello");
	assert(l.get(0, int)!! == 1);
	assert(l.pop(String)!! == "Hello");
	assert(l.pop(int)!! == 1);
	assert(l.len() == 0);
	l.free();
}