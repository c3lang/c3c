module foo;

// def <name> = <name>
def standard_foo = __stdin;
def someFunctionIntBool = someFunction<int, bool>;
def FooInt = Foo<int>;
def A_CONST_INT = A_CONST<int>;

def standard_foo<int> = ofke; // #error: Expected '='
def fn foo = fef; // #error: A type, variable, constant or attribute name was expected here
def feokfe = fn void(int); // #error: Expected a function or variable name here
def AOFKE = ofek; // #error: Expected a constant name here
def okfoe = OFKEOK; // #error: Expected a function or variable name here
def Helo = helo; // #error: A type name was expected, but this looks a variable
def Helo = OFKE; // #error: A type name was expected here
def helo = Helo; // #error: A type alias must start with an uppercase letter and contain at least one lower case letter.
def HELO = Helo; // #error: A type alias must start with an uppercase letter and contain at least one lower case letter.
