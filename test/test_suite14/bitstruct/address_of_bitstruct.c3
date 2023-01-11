bitstruct BitField : char
{
  int a : 0..2;
  int b : 4..5;
  int c : 6..7;
}

fn void test()
{
    BitField x;
    BitField* z = &x;
    x.a;
    z.a;
    &x.a; // #error: You cannot take the address of a bitstruct member
}