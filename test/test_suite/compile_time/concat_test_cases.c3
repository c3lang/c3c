fn void main(String[] args)
{
	var $x = int[2]{ 1, 2 } +++ int[2]{ 4, 5 };
	var $v = "foo" +++ "baz" +++ '!' +++ '?';
	var $b = x'403322' +++ "baz";
	var $b2 = x'40334a' +++ 55 +++ 55;
	var $b3 = x'403322' +++ char[2] { 1, 2 };
	var $b4 = x'403322' +++ char[2] { 1, 2 };
	var $b5 = "foo" +++ { 55, 57 };
	var $b6 = ((ichar[3])x'403322') +++ ichar[2] { 1, 2 };
	var $b7 = char[2] { 1, 2 } +++ "foo";
	assert($b7 == { 1, 2, 102, 111, 111});
	assert($b5 == "foo79");
	assert($b3 == $b4);
	assert($b6 == { 0x40, 0x33, 0x22, 1, 2 });
	int[4] $x2 = int[2]{ 1, 2 }+++ int[2]{ 4, 5 };
	int[4] $y = { 1, 2 } +++ { 4, 5 };
	assert($x == {1, 2, 4, 5});
	assert($x2 == {1, 2, 4, 5});
	assert($y == {1, 2, 4, 5});
	assert($v == "foobaz!?");
	assert($b == {64, 51, 34, 98, 97, 122});
	assert($b2 == {64, 51, 74, 55, 55});
	assert($b3 == {64, 51, 34, 1, 2});
}