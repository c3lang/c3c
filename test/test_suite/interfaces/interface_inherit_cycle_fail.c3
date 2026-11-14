interface SelfRef : SelfRef {} // #error: The interface 'SelfRef' cannot inherit from itself
interface CycleA : CycleB {}
interface CycleB : CycleA {} // #error: The interface 'CycleB' cannot inherit from itself
interface Other {}

fn void test()
{
	SelfRef s;
	Other o = s;
	CycleA a;
	Other p = a;
}
