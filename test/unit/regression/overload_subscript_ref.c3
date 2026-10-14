module subscript_tester;

struct Foo
{
	int a;
	Foo2 foo2;
}
fn void Foo.bar(&self) { self.a = 1; }

struct Foo2 { int a; }
fn void Foo2.bar(&self) { self.a = 1; }

fn void subscript_ref_test() @test
{
	FixedList{Foo, 16} list;
	list.push({});
	list[0].bar();
	list[0].foo2.bar();
	assert(list[0].a == 1);
	assert(list[0].foo2.a == 1);
}

struct FixedList <Type, SIZE>
{
	sz size;
	Type[SIZE] entries;
}

fn void FixedList.push(&self, Type element)
{
	self.entries[self.size++] = element;
}

fn Type FixedList.get(&self, sz index) @operator([]) @inline
{
	return self.entries[index];
}

fn Type* FixedList.get_ref(&self, sz index) @operator(&[]) @inline
{
	return &self.entries[index];
}

fn void FixedList.set(&self, sz index, Type value) @operator([]=)
{
	self.entries[index] = value;
}

fn sz FixedList.len(&self) @operator(len) @inline => self.size;

module subscript_tester2;
import std;
struct Test { int v; }

alias Fl = FixedList{FixedList{Test, 4}, 2};

fn void test_subscript_subscript() @test
{
	Fl t;
	t.push({});
	t.push({});
	t[1].push_all({{},{},{},{}});
	Test* tref = &t[1][^1];
	t[1][3].v = 99;
	assert(tref.v == 99);
	assert(t[1][3].v == 99);

	tref = &t[1][t[1].len() - 1];
	t[1][3].v = 88;
	assert(tref.v == 88);
}
