struct Test
{
  int i;
  short s1, s2;
}

extern func Test func_returning_struct();

func void loop()
{
    func_returning_struct();
}
