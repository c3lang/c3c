module test;
macro @foo($aaa) {}
fn int main(String[] args)
{
    @foo(int); // #error: must be followed by either
    return 0;
}