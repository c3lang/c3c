import std;
struct Result <WithType2>
{
	fault error;
}
macro err(fault error) <OfType> @builtin => (Result{OfType}){ .error = error };

faultdef TESTIN;

fn int main()
{
	Result{int} x = err(TESTIN);
	return 0;
}