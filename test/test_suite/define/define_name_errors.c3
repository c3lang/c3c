
typedef int = int; // #error: 'int' is the name of a built-in type and can't be used as an alias.
define main = foo; // #error: 'main' is reserved and cannot be used as an alias.

typedef hello = int; // #error: uppercase letter
typedef hello = Foo; // #error: uppercase letter

typedef HELLO = int; // #error: uppercase letter
typedef HELLO = Foo; // #error: uppercase letter