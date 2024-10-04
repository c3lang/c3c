fn void test(int* array, usz n)
{
    array[n] = 33;
    n[array] = 33; // #error: Indexing a value of type
}
