module foo;

fn void test()
{
    int i;
    switch (i)
    {
        case 15..13: // #error: The range is not valid because the first value (15) is greater than the second (13)
            i++;
            return;
    }
}