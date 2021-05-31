module foo;

// define <name> = <name>
define standard_foo = __stdin;
define someFunctionIntBool = someFunction<int, bool>;
define FooInt = Foo<int>;
define A_CONST_INT = A_CONST<int>;

define standard_foo<int> = ofke; // #error: Expected '='
define func foo = fef; // #error: An identifier was expected here.
define feokfe = func void(int); // #error: Expected a function or variable name here
define AOFKE = ofek; // #error: Expected a constant name here
define okfoe = OFKEOK; // #error: Expected a function or variable name here
define Helo = helo; // #error: A type name was expected here
define Helo = OFKE; // #error: A type name was expected here
define helo = Helo; // #error: Expected a function or variable name here
define HELO = Helo; // #error: Expected a constant name here
