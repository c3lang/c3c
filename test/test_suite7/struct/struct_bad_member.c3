struct Foo {
  void bar; // #error: Members cannot be of
}

def Void = void;
struct Foo2 {
  Void bar; // #error: Members cannot be of
}

distinct Void2 = void;
struct Foo3 {
  Void2 bar; // #error: has unknown size
}

fn void main(String[] args) {
  Foo foo;
  Foo2 foo2;
  Foo3 foo3;
}
