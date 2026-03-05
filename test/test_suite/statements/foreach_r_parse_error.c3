fn void test9()
{
    foreach_r (int a : { [2] = 1 }) foo();
    foreach_r (int a : { [2] = 2, 1 }) foo(); // #error: You’re using both named and unnamed values in the same
}