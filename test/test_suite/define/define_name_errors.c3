
alias Foo = int;

alias hello1 = int; // #error: To alias a type, the alias
alias hello2 = Foo; // #error: To alias a type, the alias

alias HELLO1 = int; // #error: To alias a type, the alias
alias HELLO2 = Foo; // #error: To alias a type, the alias