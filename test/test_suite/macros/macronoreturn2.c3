import std;
import libc;

macro int donotreach1() => unreachable();
macro donotreach2() => unreachable();
macro int donotreach3() => bye();
macro donotreach4() => bye();

fn void bye() @noreturn
{
	libc::exit(0);
}
fn int test1()
{
	donotreach1();
}

fn int test2()
{
	donotreach2();
}

fn int test3()
{
	donotreach3();
}

fn int test4()
{
	donotreach4();
}
