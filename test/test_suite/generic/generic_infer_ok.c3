import std;
struct Result @generic(WithType2)
{
	fault error;
}
macro err(fault error) @generic(OfType) @builtin => (Result{OfType}){ .error = error };

faultdef TESTIN;

fn int main()
{
	Result{int} x = err(TESTIN);
	return 0;
}