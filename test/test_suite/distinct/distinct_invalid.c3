faultdef ABC;

typedef Foo1 = Error;

typedef Foo3 = void;

typedef Foo4 = typeid;

typedef Foo = fn void(Foo** x); // #error: cannot define a new function type