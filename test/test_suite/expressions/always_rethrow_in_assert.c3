import std;
macro @test(#e1, #e2)
{
	assert(#e1 == #e2, "", io::EOF~!); // #error:  This value is always rethrown and doesn't have a definite type. This is not valid
}
fn usz foo()
{
	@test("", "");
}