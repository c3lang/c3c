fn int! bar()
{
	defer {
		(void){|
			return 4;
		|};
	}
	defer return 3; // #error: Return is not allowed inside of a defer
	return 1;
}