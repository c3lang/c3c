typedef Number2 = Number1; // #error: Recursive definition of 'Number2'
typedef Number1 = Number2;

typedef Number = Number; // #error: Recursive definition of 'Number'


typedef Loop2 = Loop; // #error: Recursive definition of 'Loop2'
typedef Loop3 = Loop2;
typedef Loop = Loop3;
