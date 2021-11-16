fn void foo(int a) {}

fn int main()
{
    foo(10, foo();  // #error: Expected the ending ')'
    return 0;
}