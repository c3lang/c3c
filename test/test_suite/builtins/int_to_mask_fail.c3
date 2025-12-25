
fn void test1()
{
	$$int_to_mask(1u, 32);
	$$int_to_mask(1u, 33); // #error: The vector length (33) cannot be greater than the bit width of the integer (32)
}

fn void test2()
{
	$$int_to_mask(1ULL, 128);
	$$int_to_mask(1ULL, 129); // #error: The vector length (129) cannot be greater than the bit width of the integer (128)
}

fn void test3()
{
	$$mask_to_int((bool[<128>]){});
	$$mask_to_int((bool[<129>]){}); // #error: Masks must be 128 or fewer bits to convert them to an integer
}