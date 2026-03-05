fn int test()
{
	int[<2>] v = { .xyz = 2 }; // #error: The 'z' component is not present in a vector of length 2, did you assume a longer vector?
	int[<2>] v2 = { .z = 2 }; // #error: The 'z' component is not present in a vector of length 2, did you assume a longer vector?
	int[<2>] v3 = { .yx = 2 }; // #error: Designated initializers using swizzling must be a contiguous range, like '.xyz = 123'
	int[<3>] v4 = { .xyz = 2 };
	int[<3>] v5 = { .xz = 2 }; // #error: Designated initializers using swizzling must be a contiguous range, like '.xyz = 123'
}
