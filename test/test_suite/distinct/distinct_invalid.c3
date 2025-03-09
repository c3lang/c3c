fault ABC;

distinct Foo1 = Error;

distinct Foo3 = void;

distinct Foo4 = typeid;

distinct Foo = fn void(Foo** x); // #error: cannot define a new function type