module test;
import std;
fn int main(String[] args)
{
	test2(test1(1));
	return 0;
}

macro String test1(a) => "abc";

macro test2(String...) // #error: must have a parameter name
{
	$for var $i = 0; $i < $vacount; $i++:
		io::printfn($vaarg[$i]);
	$endfor
}
