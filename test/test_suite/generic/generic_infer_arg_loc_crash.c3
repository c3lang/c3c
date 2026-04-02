import std;
struct Result <WithType2>
{
	fault error;
}
macro err() <WithType> => { }~ ; // #error: You cannot use the '~' operator on expressions of type 'untyped_list'
fn int main()
{
	Result{int} x = err{LinkedHashSet}()!!.$name;
}