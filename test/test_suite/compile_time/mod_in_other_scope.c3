module test;

macro void @foo(#a)
{
	#a;
}

macro void @bar(#a)
{
	$typeof(#a) x;
}
fn int main()
{

/*
	int $i;
	var $Type;
	@foo($Type = int); // noerror: only be modified
	@foo($i = 2); // noerror: only be modified
	@foo($i += 1); // noerror: only be modified
	@bar($Type = int); // noerror: only be modified
	@bar($i = 2); // noerror: only be modified
	@bar($i += 1); // noerror: only be modified*/
	return 0;
}