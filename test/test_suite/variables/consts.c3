const int foo = 3; // #error: must be all upper case

func void foo(int i)
{
    const int x = i; // #error: must be all upper case
    const int Y = i;
}