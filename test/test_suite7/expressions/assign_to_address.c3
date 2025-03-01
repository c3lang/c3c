// Issue #1833
fn int main()
{
    int* i;
    &i[0] = null; // #error: An assignable expression
    return 0;
}