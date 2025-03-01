module test;

fn void test(int[5]* x) {}
fn void test2(int* x) {}

fn int main()
{
    int[5][5] y;
    test(&y);
    test2(&y);
    return 0;
}