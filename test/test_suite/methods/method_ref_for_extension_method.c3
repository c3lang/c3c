module testc3;
import std::io;

struct Foo {
  int i;
}

fn bool Foo[].is_empty(Foo[] array) {
  return array.len == 0;
}

def foo_arr_is_empty = Foo[].is_empty;

fn void main() {
  Foo[] foos = { Foo { .i = 0 } };
  void* foo = &Foo[].is_empty;
  io::printfn("Is empty: %s", foos.is_empty());
  io::printfn("Is empty: %s", foo_arr_is_empty(foos));
}
