func void foo() {}

func int main()
{
    for (; foo() ; ) {}      // #error: 'void' into 'bool'
    return 0;
}