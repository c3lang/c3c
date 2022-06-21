define Number8 = char;
define Number32 = int;

fn void test1()
{
    int a = (ichar)(10);
    int b = (ichar)(200);
    int c = (int)(200);
    ichar d = (int)(200);     // #error: 'int' to 'ichar'
}

fn void test2()
{
    char  e = (Number32)(200);   // #error: 'Number32' (int) to 'char'
}