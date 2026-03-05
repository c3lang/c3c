module foo;
import std;

<* @param [in] ... : "ofkeofek" *> // #error: may not have any in-out modifiers
extern fn void test2(...);

<* @param ... : "ofkeofek" *>
extern fn void test(...);

<* @param ... : "ofkeofek" *> // #error: are only allowed macros and functions with
extern fn void test3();

<*
 @param ... : "ofkeofek"
 @param ... : "ofkeofek" *> // #error: @param may not be repeated
extern fn void test4(...);

fn int main()
{
	return 0;
}
