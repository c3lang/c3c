
const int CONSTANT = 1;
int[CONSTANT] a2;
int[3] a3 = { [CONSTANT] = 1 };
const bool B = true;
int[B] c2;      // #error: Expected an integer

int non_constant = 10;
int[non_constant] b;        // #error: Expected a constant value as

fn int foo()
{
    return 10;
}

int[foo()] c;               // #error: Expected a constant value as

