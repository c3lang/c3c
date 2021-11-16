
fn void test()
{
    test()?; // #error: No failable to rethrow before '?' in the expression, please remove '?'
    int i = 0;
    if (i?) return; // #error: No failable to rethrow before '?' in the expression, please remove '?'
    int! j = 0;
    if (j?) return; // #error: This expression implicitly returns with a failable result, but the function
    if ((j?)?) return; // #error: This expression implicitly returns with a failable result, but the function
}

fn void! test2()
{
    int! j = 0;
    if (j?) return;
    if ((j?)?) return; // #error: No failable to rethrow before '?' in the expression, please remove '?'
}