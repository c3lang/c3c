define Number2 = Number1; // #error: Recursive definition of 'Number2'
define Number1 = Number2;

define Number = Number; // #error: Recursive definition of 'Number'


define Loop2 = Loop; // #error: Recursive definition of 'Loop2'
define Loop3 = Loop2;
define Loop = Loop3;
