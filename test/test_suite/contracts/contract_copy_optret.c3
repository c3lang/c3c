module test;
import std;

<*
 @return? test!
*>
fn void? test2()
{
	return NOT_FOUND?;
}
<*
 @return? io::EOF, Foo!
*>
fn void? test()
{
	return NOT_FOUND?;
}
<*
	@return? NOT_FOUND
*>
alias Foo = fn void();
