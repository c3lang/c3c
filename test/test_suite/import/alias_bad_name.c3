module foo;

alias FOO = module abc; // #error: A (lower case) module name was expected here
alias hellO = module abc; // #error: The module name must be all lower case
alias hllo = module aBC; // #error: A module name may not have any
