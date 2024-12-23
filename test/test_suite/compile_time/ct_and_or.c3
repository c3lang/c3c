fn void test()
{
	$assert false ||| false ||| true ||| hello("");
	$assert !(false &&& hello(""));
	$assert !(true &&& true &&& false &&& hello(""));
}