typedef Number1 as Number2; // #error: Recursive definition of 'Number2'
typedef Number2 as Number1;

typedef Number as Number; // #error: Recursive definition of 'Number'


typedef Loop as Loop2; // #error: Recursive definition of 'Loop2'
typedef Loop2 as Loop3;
typedef Loop3 as Loop;
