struct Foo {
  void bar; // #error: Members cannot be of
}

def Void = void;
struct Foo2 {
  Void bar; // #error: Members cannot be of
}
fn void main(String[] args) {
  Foo foo;
  Foo2 foo2;
}
