module compiler_rt::banned;

fn int first() @weak @cname("__c3_test_first")
{
	tlocal int x; // #error: Thread locals may not be used in compiler-rt
	return x;
}

fn int second() @weak @cname("__c3_test_second")
{
	tlocal int a; // #error: Thread locals may not be used in compiler-rt
	return a;
}

fn int third(int x) @weak @cname("__c3_test_third")
{
	if (x)
	{
		tlocal int b; // #error: Thread locals may not be used in compiler-rt
		return b;
	}
	return 0;
}

module test;
fn void main() {}
