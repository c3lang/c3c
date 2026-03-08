module abc;
import std;


int abc @weak = 123;
double abc; // #error:  The definition for 'abc' doesn't match the replaced definition, type mismatch between 'double' and 'int'


const int ABC @weak = 123;
const ABC = 333; // #error:  The definition for 'ABC' doesn't match the replaced definition, mixing untyped and typed definitions
