struct Foo { int x; }
/**
 * @param [out] f
 **/
fn void bar(Foo* f)
{
  f.x = 123;
}

/**
 * @param [in] f
 **/
fn void foo(Foo* f)
{
  bar(f); // #error: It's not allowed to pass an 'in'
}

/**
 * @param [in] f
 **/
fn void foo2(Foo* f)
{
}

/**
 * @param [out] f
 **/
fn void baz(Foo *f)
{
	foo2(f); // #error: It's not allowed to pass an 'out'
}