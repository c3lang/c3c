interface SelfRef : SelfRef {} // #error: Recursive definition of 'SelfRef'
interface CycleA : CycleB {} // #error: Recursive definition of 'CycleA'
interface CycleB : CycleA {}
interface Other {}

fn void test()
{
	SelfRef s;
	Other o = s;
	CycleA a;
	Other p = a;
}
