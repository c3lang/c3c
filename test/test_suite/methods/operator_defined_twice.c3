distinct Foo = int;

fn int Foo.fadd(&self, int x) @operator([]) { return 1; }
fn int Foo.badd(&self, int x) @operator([]) { return 1; } // #error: This operator is already defined

fn void main()
{
	Foo x = 1;
}
