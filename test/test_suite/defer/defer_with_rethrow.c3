fn int! foo()
{ return 1; }


fn int! bar()
{
	defer {
		{|
			foo()!;
		|};
	}
	defer foo()!; // #error: Returns are not allowed inside of defers.
	return 1;
}