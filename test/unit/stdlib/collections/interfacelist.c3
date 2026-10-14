module interfacelist_test @test;
import std::collections::interfacelist;
interface Test
{
	fn int test();
}

alias TestL = InterfaceList {Test};

struct Test1 (Test)
{
	int a;
}
fn int Test1.test(&self) @dynamic => self.a;

struct Test2 (Test)
{
	String b;
}
fn int Test2.test(&self) @dynamic => (int)self.b.len;


fn void initialized() => @pool()
{
	TestL l;
	assert(!l.is_initialized());
	l.tinit();
	assert(l.is_initialized());
}

fn void basic_interation() => @pool()
{
	TestL l;
	l.push((Test1){1});
	l.push((Test1){1234});
	assert(to_ints(l) == {1, 1234});
	assert(l.pop_retained().test()!! == 1234);
	l.push((Test1){56789});
	assert(to_ints(l) == {1, 56789});
	l.set(2, (Test2){"abc"});
	assert(to_ints(l) == {1, 56789, 3});
}

fn void remove_at() => @pool()
{
	TestL l;
	for (int i = 0; i < 5; i++)
	{
		l.push((Test1){i});
	}
	assert(to_ints(l) == {0, 1, 2, 3, 4});
	l.remove_at(1);
	assert(to_ints(l) == {0, 2, 3, 4});
	l.remove_at(3);
	assert(to_ints(l) == {0, 2, 3});
}

fn void remove_with_predicate() => @pool()
{
	TestL l;
	l.push((Test1){1});
	l.push((Test1){1234});
	l.push((Test2){"wefhewoifw"});
	l.push((Test1){-1290987});
	l.push((Test2){"abc"});
	assert(to_ints(l) == {1, 1234, 10, -1290987, 3});
	l.remove_if(fn (val) => val.test() < 5);
	assert(to_ints(l) == {1234, 10});
	l.remove_if(fn (val) => val.type == Test2::typeid);
	assert(to_ints(l) == {1234});
}

fn void retain_with_predicate() => @pool()
{
	TestL l;
	l.push((Test1){1234});
	l.push((Test1){2345});
	l.push((Test1){3456});
	l.push((Test2){"abc"});
	l.push((Test2){"defg"});
	assert(to_ints(l) == {1234, 2345, 3456, 3, 4});
	l.retain_if(fn (val) => val.test() % 2 == 0);
	assert(to_ints(l) == {1234, 3456, 4});
}

fn void remove_with_test() => @pool()
{
	TestL l;
	l.push((Test1){532});
	l.push((Test2){"hello"});
	l.push((Test2){"abcdef"});
	l.push((Test1){765});
	assert(to_ints(l) == {532, 5, 6, 765});
	l.remove_using_test(fn (x, p) => x.type == p.type, &&(Test1){});
	assert(to_ints(l) == {5, 6});
	l.remove_using_test(fn (x, p) => x.test() == p.test(), &&(Test2){"abcdef"});
	assert(to_ints(l) == {5});
}

fn void retain_with_test() => @pool()
{
	TestL l;
	l.push((Test1){345});
	l.push((Test1){3535});
	l.push((Test1){7654});
	l.push((Test2){"abdef"});
	l.push((Test1){6432});
	l.push((Test1){585868});
	assert(to_ints(l) == {345, 3535, 7654, 5, 6432, 585868});
	l.retain_using_test(fn (x, p) => x.test() < p.test(), &&(Test1){1000});
	assert(to_ints(l) == {345, 5});
	l.retain_using_test(fn (x, p) => x.type == p.type && x.test() == p.test(), &&(Test1){0});
	assert(to_ints(l) == {});
}

fn void remove_if_tests_each_element_once() => @pool()
{
	TestL l;
	for (int i = 1; i <= 7; i++) l.push((Test1){i});
	call_count = 0;
	sz removed = l.remove_if(&counting_odd);
	assert(removed == 4);
	assert(to_ints(l) == {2, 4, 6});
	assert(call_count == 7);
}

fn void retain_if_tests_each_element_once() => @pool()
{
	TestL l;
	for (int i = 1; i <= 7; i++) l.push((Test1){i});
	call_count = 0;
	sz removed = l.retain_if(&counting_odd);
	assert(removed == 3);
	assert(to_ints(l) == {1, 3, 5, 7});
	assert(call_count == 7);
}

fn void remove_if_mixed_runs() => @pool()
{
	// Leading run, interior runs of different lengths and a trailing run of removals.
	TestL l;
	foreach (int v : { 1, 3, 5, 2, 4, 7, 9, 6, 11, 13, 8, 15, 17 }) l.push((Test1){v});
	call_count = 0;
	sz removed = l.remove_if(&counting_odd);
	assert(removed == 9);
	assert(to_ints(l) == {2, 4, 6, 8});
	assert(call_count == 13);
}

fn void remove_if_and_retain_if_edge_cases() => @pool()
{
	TestL l;
	// Empty list
	call_count = 0;
	assert(l.remove_if(&counting_odd) == 0);
	assert(l.retain_if(&counting_odd) == 0);
	assert(call_count == 0);
	assert(l.len() == 0);

	// Nothing to remove
	foreach (int v : { 2, 4, 6 }) l.push((Test1){v});
	call_count = 0;
	assert(l.remove_if(&counting_odd) == 0);
	assert(to_ints(l) == {2, 4, 6});
	assert(call_count == 3);

	// Remove everything
	call_count = 0;
	assert(l.retain_if(&counting_odd) == 3);
	assert(l.len() == 0);
	assert(call_count == 3);

	// Retain everything
	foreach (int v : { 1, 3, 5 }) l.push((Test1){v});
	call_count = 0;
	assert(l.retain_if(&counting_odd) == 0);
	assert(to_ints(l) == {1, 3, 5});
	assert(call_count == 3);
}

fn void remove_if_frees_removed_elements() => @pool()
{
	// The list owns copies of its elements: removed ones must be freed exactly once, kept ones not at all.
	TrackingAllocator tracker;
	tracker.init(tmem);
	defer tracker.free();
	TestL l;
	l.init(&tracker);
	foreach (int v : { 1, 2, 3, 4, 5, 6, 7, 8 }) l.push((Test1){v});
	assert(tracker.allocation_count() == 9); // 8 elements + the backing array
	sz removed = l.remove_if(&counting_odd);
	assert(removed == 4);
	assert(to_ints(l) == {2, 4, 6, 8});
	assert(tracker.allocation_count() == 5); // 4 elements + the backing array
	l.free();
	assert(tracker.allocation_count() == 0);
}

fn void retain_if_frees_removed_elements() => @pool()
{
	TrackingAllocator tracker;
	tracker.init(tmem);
	defer tracker.free();
	TestL l;
	l.init(&tracker);
	foreach (int v : { 1, 2, 3, 4, 5, 6, 7, 8 }) l.push((Test1){v});
	assert(tracker.allocation_count() == 9);
	sz removed = l.retain_if(&counting_odd);
	assert(removed == 4);
	assert(to_ints(l) == {1, 3, 5, 7});
	assert(tracker.allocation_count() == 5);
	l.free();
	assert(tracker.allocation_count() == 0);
}

module interfacelist_test;

int call_count;

fn bool counting_odd(Test x)
{
	call_count++;
	return x.test() % 2 != 0;
}

fn int[] to_ints(TestL l) => @map(tmem, l.array_view(), fn int(Test x) => x.test());

import std::core::array @public;
macro @map(Allocator alloc, array, operation)
{
	var res = alloc::alloc_array(alloc, $Typeof(operation)::returns, lengthof(array));
	foreach (i, val : array) res[i] = operation(val);
	return res;
}
