public test;

/**
 * @require parse(a = b), parse(b = a)
 */
public macro void swap(&a, &b)
{
    typeof(a) temp = a;
    a = b;
    b = temp;
}

/**
 * @require parse(a = b), parse(b = a)
 */
public macro void swap2(auto &a, auto &b)
{
    auto temp = a;
    a = b;
    b = temp;
}