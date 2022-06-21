fn void test()
{
    void* x = &x;
    int y = y; // #error: This looks like the initialization of the variable was circular.
}