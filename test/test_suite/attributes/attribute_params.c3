attrdef @Foo() = @inline; // #error: At least one parameter was expected
attrdef @Bar = @inline;

fn void test1() @Foo { }
fn void test2() @Foo() { } // #error: An expression was
fn void test3() @Bar { }
fn void test4() @Bar() { } // #error: An expression was