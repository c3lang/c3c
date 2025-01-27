enum Test2 : (usz a, usz b) {
  FOO = {4, 5, },
}

enum Test : (usz a, usz b) {
  FOO = {a: 1, b: 1}, // #error: This looks like
}