struct Foo { int x; }

fn void Foo.bar(&self = null) {} // #error: A self parameter cannot have a default value

fn void Foo.baz(self = {}) {} // #error: A self parameter cannot have a default value

macro void Foo.@m(&self = null) {} // #error: A self parameter cannot have a default value
