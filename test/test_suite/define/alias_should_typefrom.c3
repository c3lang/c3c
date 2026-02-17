import std;

const C = 2;
alias BaseType = C < 32 ??? int : long; // #error: Expected a type to alias here, if you are providing a constant typeid-expression

fn void main()
{

}