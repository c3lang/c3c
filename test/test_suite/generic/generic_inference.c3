import std::collections::linkedlist;
struct Foo
{
	LinkedList {int} x;
}

fn int main(String[] args)
{
	Foo f;
	f.x = linkedlist::@new(tmem);
	Foo f1 = { .x = linkedlist::@new(tmem) };
	return 0;
}