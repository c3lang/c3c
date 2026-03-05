bitstruct Foo : uint
{
	bool test : 2;
}

fn bool Foo.test(self, Foo other) @operator(==) => true;

fn Foo Foo.and(self, uint other) @operator(&) => (Foo)(((uint)self) & other); // #error: Bitstructs do not support
fn Foo Foo.or(self, uint other) @operator(|) => (Foo)(((uint)self) | other); // #error: Bitstructs do not support

fn void Foo.and_eq(&self, uint other) @operator(&=) => *self = (Foo)(((uint)*self) & other); // #error: Bitstructs do not support
fn void Foo.or_eq(&self, uint other) @operator(|=) => *self = (Foo)(((uint)*self) | other); // #error: Bitstructs do not support

fn Foo Foo.neg(self) @operator(~) { return {}; } // #error: Bitstructs do not support