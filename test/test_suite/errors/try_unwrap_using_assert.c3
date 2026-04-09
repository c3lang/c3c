module test;

extern fn int? maybe();

fn int tester(int n)
{
    int? num = maybe();
    assert(try num, "Hello"); // #error:  'try' can only be used in if/while conditionals or defer, it cannot be used standalone.
    int x = num;
    return num;
}
