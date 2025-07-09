
fn void test()
{
    test()!; // #error: No optional to rethrow before '!' in the expression, please remove '!'
    int i = 0;
    if (i!) return; // #error: No optional to rethrow before '!' in the expression, please remove '!'
    int? j = 0;
    if (j!) return; // #error: This expression is doing a rethrow
    if ((j!)!) return; // #error: This expression is doing a rethrow
}

fn void? test2()
{
    int? j = 0;
    if (j!) return;
    if ((j!)!) return; // #error: No optional to rethrow before '!' in the expression, please remove '!'
}