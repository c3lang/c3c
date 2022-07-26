const int foo = 3; // #error: must be all uppercase

fn void foo(int i)
{
    const int x = i; // #error: must be all uppercase
    const int Y = i;
}