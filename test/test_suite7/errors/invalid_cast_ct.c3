module exitcodes;
import std::io;

fault HadError
{
    BAD_STUFF,
    WORSE_STUFF,
    THE_WORST_STUFF
}

fn int main(String[] args)
{
	HadError err = HadError.BAD_STUFF;
	SearchResult res = (SearchResult)(anyfault)HadError.BAD_STUFF; // #error: This expression is known at compile time
	return 0;
}
