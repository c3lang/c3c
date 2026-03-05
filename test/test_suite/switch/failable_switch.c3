faultdef FOO;

fn void test()
{
    int x = 0;
    switch (x)
    {
        case FOO~ : // #error: 'int?' to 'int'
            x = x + 1;
    }
}