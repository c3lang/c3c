struct Foo {
    union Bar { // #error: name of an inner struct
        int a;
    }
}
