
define Foo = fn void(int a = 10);
fn int abc() { return 1; }
define Foo2 = fn void(int a = abc());
