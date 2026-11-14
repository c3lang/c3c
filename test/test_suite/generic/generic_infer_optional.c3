module test <Type>;

struct TestType
{
	Type a;
}

fn TestType new(Type val) => { .a = val };
fn TestType* ptr_new(TestType* val) => val;

module main;
import test;

fn TestType{int}? ret_optional()
{
	return test::new(1);
}

fn void main()
{
	TestType{int} a = test::new(1);
	TestType{int} b = test::new{int}(1);
	TestType{int}? c = test::new{int}(1);
	TestType{int}? d = test::new(1);
	d = test::new(2);
	TestType{int}*? e = test::ptr_new(&a);
	(void)ret_optional();
}
