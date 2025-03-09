fn void test()
{
    int? z;
    int? w;
    try w = z; // #error: can only be used when unwrapping an optional
}
