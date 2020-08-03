func void test()
{
    int x = 0;
    if (x < 0) x++;
    if (x <
       0) x++;
    if (x < 0
       && x < 0) x++;


    if (x < 0) x++;         // #error: if-statements with an 'else' must use '{ }' even around a single statement.
    else { x++; };

    if (x < 0)
    {
        x++;
    }
    else x++;               // #error: An 'else' must use '{ }' even around a single statement.

    if (x < 0)
    {
        x++;
    }
    else if (x > 0)
    {
        x++;
    }

}