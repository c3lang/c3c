struct Test
{
  int i;
  short s1, s2;
}

extern fn Test func_returning_struct();

fn void loop()
{
    func_returning_struct();
}
