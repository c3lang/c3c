func void foo(int a) {}

public func int main()
{
    foo(10, foo();  // #error: Expected the ending ')'
    return 0;
}