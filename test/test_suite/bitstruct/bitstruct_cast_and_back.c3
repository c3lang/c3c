// See issue #1159

bitstruct Foo : int {
  bool a;
}

bitstruct Bar : int {
  bool a;
  bool b;
}

fn void bitstruct_cast() {
  Bar bar;
  Foo foo = (Foo)(int)bar;
}
