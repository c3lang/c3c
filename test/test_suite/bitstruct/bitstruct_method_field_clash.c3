bitstruct Flags : char
{
	bool foo;
	bool bar;
}

fn void Flags.foo(self) {} // #error: already has a field with the same name
fn void Flags.bar(&self) {} // #error: already has a field with the same name
fn void Flags.baz(self) {}
