fn void foo(int a) {}

fn int main()
{
    foo(10, foo();  // #error: Did you forget a ')' before this
    return 0;
}