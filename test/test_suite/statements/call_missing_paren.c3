func void foo(int a) {}

func int main()
{
    foo(10, foo();  // #error: Expected the ending ')'
    return 0;
}