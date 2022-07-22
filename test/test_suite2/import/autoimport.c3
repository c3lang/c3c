module foo;

fn int test() @builtin
{
    return 1;
}

macro int testm() @builtin
{
    return 1;
}

fn int test1()
{
    return 1;
}

macro int testm1()
{
    return 1;
}

module bar;
import foo;

fn void main()
{
    testm();
    test();
}

fn void main2()
{
    testm1(); // #error: Macros from other modules must be prefixed with the module nam
    test1();  // #error: Functions from other modules must be prefixed with the module nam
}