fn void test9()
{
    foreach (int a : { [2] = 1 }) foo();
    foreach (int a : { [2] = 2, 1 }) foo(); // #error: You’re using both named and unnamed values in the same
}