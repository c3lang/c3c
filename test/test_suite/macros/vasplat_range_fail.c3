macro int @from_end(...) { int[*] x = { 7, ...$vaarg[^5..] }; return x.len; } // #error: Start index exceeds the number of parameters (2)
macro int @reversed(...) { int[*] x = { 7, ...$vaarg[2..0], 8 }; return x.len; } // #error: The end index is before the start index
macro int @past_end(...) { int[*] x = { 7, ...$vaarg[0..2] }; return x.len; } // #error: End index would exceed the number of parameters
macro int @end_from_end(...) { int[*] x = { 7, ...$vaarg[..^3] }; return x.len; } // #error: End index exceeds the number of parameters (2)
enum Foo { A, B }
macro Foo @typecall(...) => Foo::from_ordinal(...$vaarg[^5..]); // #error: Start index exceeds the number of parameters (2)
macro uptr @builtin_call(...) @builtin => $$syscall( // #warning: Builtins are intended for internal use
	...$vaarg[^5..]); // #error: Start index exceeds the number of parameters (2)

fn void test1() { @from_end(1, 2); }
fn void test2() { @reversed(1, 2, 3); }
fn void test3() { @past_end(1, 2); }
fn void test4() { @end_from_end(1, 2); }
fn void test5() { @typecall(1, 2); }
fn void test6() { @builtin_call(1, 2); }
