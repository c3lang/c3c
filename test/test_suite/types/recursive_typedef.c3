alias Number2 = Number1; // #error: Recursive definition of 'Number2'
alias Number1 = Number2;

alias Number = Number; // #error: Recursive definition of 'Number'


alias Loop2 = Loop; // #error: Recursive definition of 'Loop2'
alias Loop3 = Loop2;
alias Loop = Loop3;
