module app;

typedef Foo = int;
fn void Foo.method(&self)
{
	self.method.anything = null; // #error: There is no member or method
}

fn int main()
{
	Foo bar;
	bar.method();
	return 0;
}