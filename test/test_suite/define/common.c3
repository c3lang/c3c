module foo;

// def <name> = <name>
alias standard_foo = __stdin;
alias someFunctionIntBool = someFunction{int, bool};
alias FooInt = Foo{int};
alias A_CONST_INT = A_CONST{int};

alias standard_foo{int} = ofke; // #error: Expected '='
alias fn foo = fef; // #error: A type, variable, constant or attribute name was expected here
alias feokfe = fn void(int); // #error: This looks like you're declaring a function type alias
alias Helo = helo; // #error: An identifier may not be aliased
alias Helo = OFKE; // #error: A constant may not have a type name alias
alias int = int; // #error: 'int' is a reserved keyword, try another name
alias main = foo; // #error: 'main' is reserved and cannot be used as an alias.
