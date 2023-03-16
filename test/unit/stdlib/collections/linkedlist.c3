module linkedlist_test;
import std::collections::linkedlist;

typedef IntList = LinkedList<int>;

fn void! test_push() @test
{
	IntList list;
	list.push(23);
	assert(list.len() == 1);
	assert(list.first()? == 23);
	assert(list.last()? == 23);
	list.push(55);
	assert(list.len() == 2);
	assert(list.last()? == 23);
	assert(list.first()? == 55);
}

fn void! test_push_last() @test
{
	IntList list;
	list.push_last(23);
	assert(list.len() == 1);
	assert(list.first()? == 23);
	assert(list.last()? == 23);
	list.push_last(55);
	assert(list.len() == 2);
	assert(list.last()? == 55);
	assert(list.first()? == 23);
}

fn void! test_get() @test
{
	IntList list;
	list.push(23);
	list.push(55);
	list.push(-3);
	assert(list.get(0) == -3);
	assert(list.get(1) == 55);
	assert(list.get(2) == 23);
}

fn void! test_insert() @test
{
	IntList list;
	list.push(23);
	list.push(55);
	list.push(-3);
	list.insert(0, 1);
	list.insert(2, 11);
	list.insert(4, 111);
	list.insert(6, 1111);
	assert(list.get(0) == 1);
	assert(list.get(1) == -3);
	assert(list.get(2) == 11);
	assert(list.get(3) == 55);
	assert(list.get(4) == 111);
	assert(list.get(5) == 23);
	assert(list.get(6) == 1111);
}

fn void! test_set() @test
{
	IntList list;
	list.push(23);
	list.push(55);
	list.push(-3);
	for (int i = 0; i < 3; i++) list.set(i, list.get(i) + 1);
	assert(list.get(0) == -2);
	assert(list.get(1) == 56);
	assert(list.get(2) == 24);
}

fn void! test_remove() @test
{
	IntList list;
	for (int i = 0; i < 10; i++) list.push(i);
	list.remove(0);
	list.remove(1);
	list.remove(7);
	list.remove(5);
	assert(list.get(0) == 8);
	assert(list.get(1) == 6);
	assert(list.get(5) == 1);
	assert(list.get(4) == 3);
}
fn void! test_remove_value() @test
{
	IntList list;
	list.push(23);
	list.push(55);
	list.push(-3);
	assert(list.remove_value(23));
	assert(list.pop()? == -3);
	assert(list.pop()? == 55);
	assert(!list.len());

	list.push(23);
	list.push(55);
	list.push(-3);
	assert(list.remove_value(55));
	assert(list.pop()? == -3);
	assert(list.pop()? == 23);
	assert(!list.len());

	list.push(23);
	list.push(55);
	list.push(-3);
	assert(list.remove_value(-3));
	assert(list.pop()? == 55);
	assert(list.pop()? == 23);
	assert(!list.len());
}

fn void! test_remove_last_value() @test
{
	IntList list;
	list.push(23);
	list.push(55);
	list.push(-3);
	assert(list.remove_last_value(23));
	assert(list.pop()? == -3);
	assert(list.pop()? == 55);
	assert(!list.len());

	list.push(23);
	list.push(55);
	list.push(-3);
	assert(list.remove_last_value(55));
	assert(list.pop()? == -3);
	assert(list.pop()? == 23);
	assert(!list.len());

	list.push(23);
	list.push(55);
	list.push(-3);
	assert(list.remove_last_value(-3));
	assert(list.pop()? == 55);
	assert(list.pop()? == 23);
	assert(!list.len());
}

fn void! test_pop() @test
{
	IntList list;
	list.push(23);
	list.push(55);
	list.push(-3);
	assert(list.len() == 3);
	assert(list.last()? == 23);
	assert(list.first()? == -3);
	assert(list.pop()? == -3);
	assert(list.len() == 2);
	assert(list.last()? == 23);
	assert(list.first()? == 55);
	assert(list.pop()? == 55);
	assert(list.last()? == 23);
	assert(list.first()? == 23);
	assert(list.pop()? == 23);
	assert(list.len() == 0);
	assert(catch? list.pop());
	assert(list.len() == 0);
	list.push(55);
	assert(list.len() == 1);
}

fn void! test_remove_first() @test
{
	IntList list;
	list.push(23);
	list.push(55);
	list.push(-3);
	assert(list.len() == 3);
	assert(list.last()? == 23);
	assert(list.first()? == -3);
	assert(try? list.remove_first());
	assert(list.len() == 2);
	assert(list.last()? == 23);
	assert(list.first()? == 55);
	assert(try? list.remove_first());
	assert(list.last()? == 23);
	assert(list.first()? == 23);
	assert(try? list.remove_first());
	assert(list.len() == 0);
	assert(catch? list.pop());
	assert(list.len() == 0);
	list.push(55);
	assert(list.len() == 1);
}

fn void! test_remove_last() @test
{
	IntList list;
	list.push(23);
	list.push(55);
	list.push(-3);
	assert(list.len() == 3);
	assert(list.last()? == 23);
	assert(list.first()? == -3);
	assert(try? list.remove_last());
	assert(list.len() == 2);
	assert(list.first()? == -3);
	assert(list.last()? == 55);
	assert(try? list.remove_last());
	assert(list.first()? == -3);
	assert(list.last()? == -3);
	assert(try? list.remove_last());
	assert(list.len() == 0);
	assert(catch? list.remove_last());
	assert(list.len() == 0);
	list.push(55);
	assert(list.len() == 1);
}