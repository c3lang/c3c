macro bar() @const
{
	return; // #error: provide a value
}

macro void barz() @const // #error: return a constant
{
	return 1;
}

faultdef CONST_MACRO_FAIL;

macro void? opt_void($x) @const // #error: may not return 'void?'
{
	$if $x > 5:
		return CONST_MACRO_FAIL~;
	$endif
}

macro int? opt_int($x) @const
{
	return $x;
}

fn void main()
{
	int? x = opt_int(1);
}
