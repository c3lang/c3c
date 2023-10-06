def Number8 = char;
def Number32 = int;
distinct DNumber32 = int;
fn void test1()
{
    int a = (ichar)(10);
    int b = (ichar)(200);
    int c = (int)(200);
    ichar d = (int)(200);     // #error: truncate the value
}

fn void test2()
{
    char e = (Number32)(200);
    char f = (DNumber32)(200); // #error: DNumber32
}