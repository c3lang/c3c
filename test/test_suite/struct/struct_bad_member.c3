struct Foo {
  void bar; // #error: Members cannot be of
}

fn void main(String[] args) {
  Foo foo;
}
