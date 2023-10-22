fn void test()
{
	$assert $or(false, false, true, hello(""));
	$assert !$and(false, hello(""));
	$assert !$and(true, true, false, hello(""));
}