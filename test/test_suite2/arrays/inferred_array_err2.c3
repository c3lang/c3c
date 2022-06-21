fn void test()
{
    int[3] z;
    (int[*])(z); // #error: Inferred array types can only be used in declarations with initializers
}