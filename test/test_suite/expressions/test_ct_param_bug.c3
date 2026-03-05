module test;
macro @foo($aaa) {}
fn int main(String[] args)
{
    @foo(int);
    return 0;
}