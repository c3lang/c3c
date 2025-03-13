
alias Foo = fn void(int a = 10);
fn int abc() { return 1; }
alias Foo2 = fn void(int a = abc());
