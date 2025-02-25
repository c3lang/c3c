
fn void test() @wasm("hello", "world") { } // #error: Specifying a wasm import module
extern fn void test2() @wasm("a", "b", "c"); // #error: Too many arguments to
extern fn void test3() @extern("hello") @wasm("a"); // #error: An external name
extern fn void test4() @extern("hello") @wasm("a", "b"); // #error: An external name