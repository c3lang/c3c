// #target: macos-x64

fn void! swizzle_test()
{
	int[<4>] abc = { 1, 2, 3, 4 };
	int[<3>] z = abc.rbx; // #error: Mixing
	int[<2>] gh;
	int[<2>] uh = gh.xz; // #error: component is not present

}



fn int main()
{
  int[<2>] $test = {3, 3};
  int[<2>] $test2 = {4, 4};
  $if ($test2.x != $test.x):
  y++; // #error: did you spell it
  $endif
  return 0;
}