import std;
struct Result <WithType2>
{
	fault error;
}
macro err(fault error) <OfType, SOME_CONST> @builtin => (Result{OfType}){ .error = error };

faultdef TESTIN;

fn int main()
{
	Result{int} x = err(TESTIN); // #error:  Found 'err' in the module 'generic_infer_mismatch', but it lacks parameters
	return 0;
}