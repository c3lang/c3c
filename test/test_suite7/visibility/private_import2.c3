module test;

fn void bar() @private
{
}
fn void bar2() @local
{
}

module baz;
import test @public;

fn void abc()
{
	test::bar();
	test::bar2(); // #error: could not be found
}

module baz2;

import test;

fn void abc()
{
	test::bar(); // #error: is '@private'
	test::bar2(); // #error: could not be found
}