import std;
struct Result @generic(WithType2)
{
	fault error;
}
macro err(fault error) @generic(OfType, SOME_CONST) @builtin => (Result{OfType}){ .error = error };

faultdef TESTIN;

fn int main()
{
	Result{int} x = err(TESTIN); // #error:  Found 'err' in the module 'generic_infer_mismatch', but it lacks parameters
	return 0;
}