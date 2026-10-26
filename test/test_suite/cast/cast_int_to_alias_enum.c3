module test;

enum Foo { A, B, C }
alias Bar = Foo;
alias Baz = Bar;

// An alias of an enum casts from a constant int exactly like the enum itself.
fn void plain()
{
	Foo f = (Foo)1;
}

fn void via_alias()
{
	Bar b = (Bar)1;
}

fn void via_alias_of_alias()
{
	Baz z = (Baz)2;
}

// The range check uses the enum's real number of values, whichever name is used.
fn void too_big_plain()
{
	Foo f = (Foo)5; // #error: The value '5' exceeds the max ordinal '2'
}

fn void too_big_alias()
{
	Bar b = (Bar)5; // #error: The value '5' exceeds the max ordinal '2'
}

fn void too_big_alias_of_alias()
{
	Baz z = (Baz)3; // #error: The value '3' exceeds the max ordinal '2'
}

fn void negative_alias()
{
	Bar b = (Bar)-1; // #error: negative number
}
