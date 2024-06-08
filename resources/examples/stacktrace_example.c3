fn void main() {
  foo();
}

fn void foo() {
  bar();
}

macro bar() {
  unreachable();
}
