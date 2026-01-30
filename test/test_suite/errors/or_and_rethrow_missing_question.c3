module faults;
faultdef
    EXAMPLE,
    DOG_ATE_MY_HOMEWORK,
    MY_TEXTBOOK_CAUGHT_FIRE,
    DISTRACTED_BY_CAT_PICTURES;


module testing_v0_7;
import faults;
import std::io;

fn int main(String[] args)
{

	int? foo = faults::EXAMPLE~;

	if (catch foo)
	{
		// This is issue 2036
		foo = foo ?? faults::DISTRACTED_BY_CAT_PICTURES; // #error: did you perhaps forget a '?'
	}

	return 0;
}