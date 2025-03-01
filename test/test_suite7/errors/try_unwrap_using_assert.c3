module test;

extern fn int! maybe();

fn int tester(int n)
{
    int! num = maybe();
    assert(try num, "Hello"); // #error: An expression was expected
    int x = num;
    return num;
}
