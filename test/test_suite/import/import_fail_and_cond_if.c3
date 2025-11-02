module test;
const int FOO = 5;
fn int main()
{
	return 0;
}

module foo @if(!$feature(XYZ));
alias bar = module thirdparty; // #error: No module named 'thirdparty' could be found, did you type the name right?

module foo @if(!$feature(XYZ));
import testme;                 // #error: No module named 'testme' could be found