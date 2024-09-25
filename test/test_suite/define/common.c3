module foo;

// def <name> = <name>
def standard_foo = __stdin;
def someFunctionIntBool = someFunction(<int, bool>);
def FooInt = Foo(<int>);
def A_CONST_INT = A_CONST(<int>);

def standard_foo(<int>) = ofke; // #error: Expected '='
def fn foo = fef; // #error: A type, variable, constant or attribute name was expected here
def feokfe = fn void(int); // #error: This looks like you're declaring a function type alias
def Helo = helo; // #error: An identifier may not be aliased
def Helo = OFKE; // #error: A constant may not have a type name alias
def int = int; // #error: 'int' is a reserved keyword, try another name
def main = foo; // #error: 'main' is reserved and cannot be used as an alias.
