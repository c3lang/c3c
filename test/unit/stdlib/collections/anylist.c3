module anylist_test @test;
import std::collections::anylist;

fn void pop() => @pool()
{
	AnyList l;
	l.push(1.0);
	l.push(1);
	l.push("hello");
	assert(l.pop(String)!! == "hello");
	assert(l.pop(int)!! == 1);
	assert(l.copy_pop(tmem)!!.type == double.typeid);
}

fn void predicates() => @pool()
{
	AnyList l;
	l.push(123u);
	l.push(-1);
	l.push("abc");
	l.push(5.0);
	l.remove_using_test(fn (x, p) => x.type == p.type, &&456u);
	assert(l[0].type == int.typeid);
	assert(l.get(0, int)!! == -1);
	l.retain_if(fn (x) => x.type == double.typeid);
	assert(l.len() == 1);
	assert(l.get(0, double)!! == 5.0);
}
