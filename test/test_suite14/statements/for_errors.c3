fn void foo() {}

fn int main()
{
    for (; foo() ; ) {}      // #error: Expected a boolean expression
    return 0;
}