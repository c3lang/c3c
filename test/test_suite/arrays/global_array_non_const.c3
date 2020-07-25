
// TODO
//const int CONSTANT = 1;
//int[CONSTANT] a;

//const bool B = true;
//i32[B] c;      // @error{size of array has non-integer type 'bool'}

int non_constant = 10;
int[non_constant] b;        // #error: Expected a constant value as

func int foo()
{
    return 10;
}

int[foo()] c;               // #error: Expected a constant value as

