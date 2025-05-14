fn void test()
{
	$assert false ||| false ||| true ||| hello("");
	$assert !(false &&& hello(""));
	$assert !(true &&& true &&& false &&& hello(""));
	int y;
	bool x = true &&& y > 0;
	bool z = true ||| y > 0;
}