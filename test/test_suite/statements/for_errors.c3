func void foo() {}

func int main()
{
    for (; foo() ; ) {}      // #error: Cannot implicitly cast 'void' to 'bool'
    return 0;
}