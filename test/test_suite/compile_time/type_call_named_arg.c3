struct Foo { int a; }
enum Col { RED, BLUE }

fn void has_tag_ct()
{
	$if int::has_tag(x: 0): // #error: Named arguments are not allowed in builtin calls
	$endif
}

fn void has_tag_runtime()
{
	bool b = Foo::has_tag(x: "a"); // #error: Named arguments are not allowed in builtin calls
}

fn void get_tag()
{
	var $t = Foo::get_tag(x: "a"); // #error: Named arguments are not allowed in builtin calls
}

fn void from_ordinal()
{
	Col c = Col::from_ordinal(x: 1); // #error: Named arguments are not allowed in builtin calls
}

fn void valid()
{
	bool b = Foo::has_tag("a") || int::has_tag("b");
	Col c = Col::from_ordinal(1);
}
