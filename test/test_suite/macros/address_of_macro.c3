import std::io;
// Issue #2049
struct Container
{
	int[] items;
}

macro int Container.get(&this, usz i) @operator([])
{
	return this.items[i];
}

fn int Container.get2(&this, usz i)
{
	return this.items[i];
}

alias ProcGetItem = fn int(usz);
fn void process(int input, ProcGetItem getter)
{
}

fn void main()
{
	Container c =  { .items = { 2, 3, 4, 5 } };
    process(10, &c.get); // #error: It's not possible to take the address

}