fn void test(int* array, usize n)
{
    array[n] = 33;
    n[array] = 33; // #error: Cannot index
}
