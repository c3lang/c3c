fn void test9()
{
    foreach (int a : { [2] = 1 }) foo();
    foreach (int a : { [2] = 2, 1 }) foo(); // #error: Normal initialization cannot be mixed with designated initialization.
}