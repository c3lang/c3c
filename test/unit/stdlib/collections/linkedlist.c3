module linkedlist_test @test;
import std::collections::linkedlist;

alias IntList = LinkedList{int};

fn void test_is_initialized()
{
	IntList test;	
	test::@check(!test.is_initialized());
	test.init(mem);
	test::@check(test.is_initialized());
	test.free();
}

fn void test_push_front()
{
	IntList list;
	defer list.free();
	list.push_front(23);
	test::eq(list.len(), 1);
	test::eq(list.first()!!, 23);
	test::eq(list.last()!!, 23);
	list.push_front(55);
	test::eq(list.len(), 2);
	test::eq(list.last()!!, 23);
	test::eq(list.first()!!, 55);

}

fn void test_push()
{
	IntList list;
	defer list.free();
	list.push(23);
	test::eq(list.len(), 1);
	test::eq(list.first()!!, 23);
	test::eq(list.last()!!, 23);
	list.push(55);
	test::eq(list.len(), 2);
	test::eq(list.last()!!, 55);
	test::eq(list.first()!!, 23);
}

fn void test_get()
{
	IntList list;
	defer list.free();
	list.push_all({ 23, 55, -3 });
	test::eq(list.get(2), -3);
	test::eq(list.get(1), 55);
	test::eq(list.get(0), 23);
	test::eq(list[0], 23);
}

fn void test_insert()
{
	IntList list;
	defer list.free();
	list.push_all({ -3, 55, 23 });
	list.insert_at(0, 1);
	list.insert_at(2, 11);
	list.insert_at(4, 111);
	list.insert_at(6, 1111);
	test::eq(list.get(0), 1);
	test::eq(list.get(1), -3);
	test::eq(list.get(2), 11);
	test::eq(list.get(3), 55);
	test::eq(list.get(4), 111);
	test::eq(list.get(5), 23);
	test::eq(list.get(6), 1111);
}

fn void test_set()
{
	IntList list;
	defer list.free();
	list.push_all({ -3, 55, 23 });
	for (int i = 0; i < 3; i++) list.set(i, list.get(i) + 1);
	test::eq(list.get(0), -2);
	test::eq(list.get(1), 56);
	test::eq(list.get(2), 24);
}

fn void test_remove_at()
{
	IntList list;
	defer list.free();
	for (int i = 0; i < 10; i++) list.push(i);
	list.remove_at(0);
	list.remove_at(1);
	list.remove_at(7);
	list.remove_at(5);
	test::eq(list.get(0), 1);
	test::eq(list.get(1), 3);
	test::eq(list.get(5), 8);
	test::eq(list.get(4), 6);
}

fn void test_remove()
{
	IntList list;
	defer list.free();
	list.push(2);
	for (int i = 0; i < 10; i++) list.push(5);
	list.push(2);
	list.remove(5);
	test::eq(list.len(), 2);
}

fn void test_remove_first_match()
{
	IntList list;
	defer list.free();
	list.push_all({ 23, 55, -3 });
	test::@check(list.remove_first_match(23));
	test::eq(list.pop()!!, -3);
	test::eq(list.pop()!!, 55);
	test::@check(!list.len());

	list.push_all({ 23, 55, -3 });
	test::@check(list.remove_first_match(55));
	test::eq(list.pop()!!, -3);
	test::eq(list.pop()!!, 23);
	test::@check(!list.len());

	list.push_all({ 23, 55, -3 });
	test::@check(list.remove_first_match(-3));
	test::eq(list.pop()!!, 55);
	test::eq(list.pop()!!, 23);
	test::@check(!list.len());
}

fn void test_remove_last_match()
{
	IntList list;
	defer list.free();
	list.push_all({ 23, 55, -3 });
	test::@check(list.remove_last_match(23));
	test::eq(list.pop()!!, -3);
	test::eq(list.pop()!!, 55);
	test::@check(!list.len());

	list.push_all({ 23, 55, -3 });
	test::@check(list.remove_last_match(55));
	test::eq(list.pop()!!, -3);
	test::eq(list.pop()!!, 23);
	test::@check(!list.len());

	list.push_all({ 23, 55, -3 });
	test::@check(list.remove_last_match(-3));
	test::eq(list.pop()!!, 55);
	test::eq(list.pop()!!, 23);
	test::@check(!list.len());
}

fn void test_pop()
{
	IntList list;
	defer list.free();
	list.push_all({ 23, 55, -3 });
	test::eq(list.len(), 3);
	test::eq(list.first()!!, 23);
	test::eq(list.last()!!, -3);
	test::eq(list.pop()!!, -3);
	test::eq(list.len(), 2);
	test::eq(list.first()!!, 23);
	test::eq(list.last()!!, 55);
	test::eq(list.pop()!!, 55);
	test::eq(list.first()!!, 23);
	test::eq(list.last()!!, 23);
	test::eq(list.pop()!!, 23);
	test::eq(list.len(), 0);
	test::@check(@catch(list.pop()));
	test::eq(list.len(), 0);
	list.push(55);
	test::eq(list.len(), 1);
}

fn void test_remove_last()
{
	IntList list;
	defer list.free();
	list.push_all({ 23, 55, -3 });
	test::eq(list.len(), 3);
	test::eq(list.first()!!, 23);
	test::eq(list.last()!!, -3);
	test::@check(@ok(list.remove_last()));
	test::eq(list.len(), 2);
	test::eq(list.first()!!, 23);
	test::eq(list.last()!!, 55);
	test::@check(@ok(list.remove_last()));
	test::eq(list.first()!!, 23);
	test::eq(list.last()!!, 23);
	test::@check(@ok(list.remove_last()));
	test::eq(list.len(), 0);
	test::@check(@catch(list.pop()));
	test::eq(list.len(), 0);
	list.push(55);
	test::eq(list.len(), 1);
}

fn void test_remove_first()
{
	IntList list;
	defer list.free();
	list.push_all({ 23, 55, -3 });
	test::eq(list.len(), 3);
	test::eq(list.first()!!, 23);
	test::eq(list.last()!!, -3);
	test::@check(@ok(list.remove_first()));
	test::eq(list.len(), 2);
	test::eq(list.last()!!, -3);
	test::eq(list.first()!!, 55);
	test::@check(@ok(list.remove_first()));
	test::eq(list.last()!!, -3);
	test::eq(list.first()!!, -3);
	test::@check(@ok(list.remove_first()));
	test::eq(list.len(), 0);
	test::@check(@catch(list.remove_first()));
	test::eq(list.len(), 0);
	list.push(55);
	test::eq(list.len(), 1);
}

fn void test_push_all_ordering()
{
	IntList l;
	defer l.free();
	l.push_all({ 23, 45, 66 });
	test::eq(l[0], 23);
	test::eq(l[1], 45);
	test::eq(l[2], 66);

	IntList l2;
	defer l2.free();
	l2.push_all({ 23, 45, 66 });
	l2.push_front_all({ 3, 6 });
	test::eq(l2[0], 3);
	test::eq(l2[1], 6);
	test::eq(l2[2], 23);
	test::eq(l2[3], 45);
	test::eq(l2[4], 66);
}

fn void test_index_of()
{
	IntList list;
	defer list.free();
	list.push_all({ 23, 55, 55, -3 });
	test::@error(list.index_of(20), NOT_FOUND);
	test::eq(list.index_of(55)!!, 1);
	test::eq(list.rindex_of(55)!!, 2);
}

fn void test_operators()
{
	IntList list;
	defer list.free();
	list.push_all({ 17, 109, 2, 8 });
	foreach (i, &e : list) *e += 2;
	test::eq(list[0], 19);
	test::eq(list[1], 111);
	test::eq(list[2], 4);
	test::eq(list[3], 10);
}