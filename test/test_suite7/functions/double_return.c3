module double_return;

fn int test(bool pos, bool color)
{
  return 0;
  return (int)(pos && color); // #warning: never execute
}

