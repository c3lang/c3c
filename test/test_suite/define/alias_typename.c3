typeid abc;
const FOO = 1;
const typeid BAR = int;
alias IsFine = BAR;
alias ShouldNotBeUppercase = FOO; // #error: An upper-case alias must always alias a type.
alias ShouldNotBeUppercase2 = abc; // #error: A type alias requires a type or a constant typeid. This is a runtime value.
