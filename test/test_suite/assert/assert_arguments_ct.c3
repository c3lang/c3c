import std;
macro @test(#e1, #e2) {
    assert(
        #e1 == #e2,
        "Assertion '" +++ $stringify(#e1) +++ " == " +++ $stringify(#e2)
        +++ "' failed, got '%s', expected '%s'.", #e1, #e2 // #error: untyped list cannot be inferred
    );
}

fn usz[<2>] grapheme_length(char*)
{
	return { 1, 1 };
}

fn void main()
{
    @test(grapheme_length("a"), { 1, 1 });
}