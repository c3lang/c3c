module abc;

alias @Foo = { @inline };

module bar;

fn void test() abc::@Foo {} // #error: Did you mean the attribute
