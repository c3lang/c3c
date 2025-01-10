module linkedlist_test @test;
import std::collections::linkedlist;

def IntList = LinkedList(<int>);

fn void test_push_front()
{
	IntList list;
	list.push_front(23);
	assert(list.len() == 1);
	assert(list.first()!! == 23);
	assert(list.last()!! == 23);
	list.push_front(55);
	assert(list.len() == 2);
	assert(list.last()!! == 23);
	assert(list.first()!! == 55);
}

fn void test_push()
{
	IntList list;
	list.push(23);
	assert(list.len() == 1);
	assert(list.first()!! == 23);
	assert(list.last()!! == 23);
	list.push(55);
	assert(list.len() == 2);
	assert(list.last()!! == 55);
	assert(list.first()!! == 23);
}

fn void test_get()
{
	IntList list;
	list.push(23);
	list.push(55);
	list.push(-3);
	assert(list.get(2) == -3);
	assert(list.get(1) == 55);
	assert(list.get(0) == 23);
}

fn void test_insert()
{
	IntList list;
	list.push(-3);
	list.push(55);
	list.push(23);
	list.insert_at(0, 1);
	list.insert_at(2, 11);
	list.insert_at(4, 111);
	list.insert_at(6, 1111);
	assert(list.get(0) == 1);
	assert(list.get(1) == -3);
	assert(list.get(2) == 11);
	assert(list.get(3) == 55);
	assert(list.get(4) == 111);
	assert(list.get(5) == 23);
	assert(list.get(6) == 1111);
}

fn void test_set()
{
	IntList list;
    list.push(-3);
	list.push(55);
	list.push(23);
	for (int i = 0; i < 3; i++) list.set(i, list.get(i) + 1);
	assert(list.get(0) == -2);
	assert(list.get(1) == 56);
	assert(list.get(2) == 24);
}

fn void test_remove_at()
{
	IntList list;
	for (int i = 0; i < 10; i++) list.push(i);
	list.remove_at(0);
	list.remove_at(1);
	list.remove_at(7);
	list.remove_at(5);
	assert(list.get(0) == 1);
	assert(list.get(1) == 3);
	assert(list.get(5) == 8);
	assert(list.get(4) == 6);
}

fn void test_remove()
{
	IntList list;
	list.push(2);
	for (int i = 0; i < 10; i++) list.push(5);
	list.push(2);
	list.remove(5);
	assert(list.len() == 2);
}

fn void test_remove_first_match()
{
	IntList list;
	list.push(23);
	list.push(55);
	list.push(-3);
	assert(list.remove_first_match(23));
	assert(list.pop()!! == -3);
	assert(list.pop()!! == 55);
	assert(!list.len());

	list.push(23);
	list.push(55);
	list.push(-3);
	assert(list.remove_first_match(55));
	assert(list.pop()!! == -3);
	assert(list.pop()!! == 23);
	assert(!list.len());

	list.push(23);
	list.push(55);
	list.push(-3);
	assert(list.remove_first_match(-3));
	assert(list.pop()!! == 55);
	assert(list.pop()!! == 23);
	assert(!list.len());
}

fn void test_remove_last_match()
{
	IntList list;
	list.push(23);
	list.push(55);
	list.push(-3);
	assert(list.remove_last_match(23));
	assert(list.pop()!! == -3);
	assert(list.pop()!! == 55);
	assert(!list.len());

	list.push(23);
	list.push(55);
	list.push(-3);
	assert(list.remove_last_match(55));
	assert(list.pop()!! == -3);
	assert(list.pop()!! == 23);
	assert(!list.len());

	list.push(23);
	list.push(55);
	list.push(-3);
	assert(list.remove_last_match(-3));
	assert(list.pop()!! == 55);
	assert(list.pop()!! == 23);
	assert(!list.len());
}

fn void test_pop()
{
	IntList list;
	list.push(23);
	list.push(55);
	list.push(-3);
	assert(list.len() == 3);
	assert(list.first()!! == 23);
	assert(list.last()!! == -3);
	assert(list.pop()!! == -3);
	assert(list.len() == 2);
	assert(list.first()!! == 23);
	assert(list.last()!! == 55);
	assert(list.pop()!! == 55);
	assert(list.first()!! == 23);
	assert(list.last()!! == 23);
	assert(list.pop()!! == 23);
	assert(list.len() == 0);
	assert(@catch(list.pop()));
	assert(list.len() == 0);
	list.push(55);
	assert(list.len() == 1);
}

fn void test_remove_last()
{
	IntList list;
	list.push(23);
	list.push(55);
	list.push(-3);
	assert(list.len() == 3);
	assert(list.first()!! == 23);
	assert(list.last()!! == -3);
	assert(@ok(list.remove_last()));
	assert(list.len() == 2);
	assert(list.first()!! == 23);
	assert(list.last()!! == 55);
	assert(@ok(list.remove_last()));
	assert(list.first()!! == 23);
	assert(list.last()!! == 23);
	assert(@ok(list.remove_last()));
	assert(list.len() == 0);
	assert(@catch(list.pop()));
	assert(list.len() == 0);
	list.push(55);
	assert(list.len() == 1);
}

fn void test_remove_first()
{
	IntList list;
	list.push(23);
	list.push(55);
	list.push(-3);
	assert(list.len() == 3);
	assert(list.first()!! == 23);
	assert(list.last()!! == -3);
	assert(@ok(list.remove_first()));
	assert(list.len() == 2);
	assert(list.last()!! == -3);
	assert(list.first()!! == 55);
	assert(@ok(list.remove_first()));
	assert(list.last()!! == -3);
	assert(list.first()!! == -3);
	assert(@ok(list.remove_first()));
	assert(list.len() == 0);
	assert(@catch(list.remove_first()));
	assert(list.len() == 0);
	list.push(55);
	assert(list.len() == 1);
}