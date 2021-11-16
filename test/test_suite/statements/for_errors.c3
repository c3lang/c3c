fn void foo() {}

fn int main()
{
    for (; foo() ; ) {}      // #error: 'void' into 'bool'
    return 0;
}