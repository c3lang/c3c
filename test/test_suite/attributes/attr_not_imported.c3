module abc;

define @Foo = { @inline };

module bar;

fn void test() abc::@Foo {} // #error: Did you mean the attribute
