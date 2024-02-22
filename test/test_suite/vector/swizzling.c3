// #target: macos-x64

fn void! swizzle_test()
{
	int[<4>] abc = { 1, 2, 3, 4 };
	int[<3>] z = abc.rbx; // #error: Mixing
	int[<2>] gh;
	int[<2>] uh = gh.xz; // #error: component is not present

}

