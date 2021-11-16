fn void test()
{
    int! z;
    int! w;
    try w = z; // #error: An unwrapping 'try' can only occur as the last element of a conditional, did you want 'try(expr)
}
