struct Foo { int x; }
<* @param [out] f *>
fn void bar(Foo* f)
{
  f.x = 123;
}

<* @param [in] fo *> // #error: There is no parameter 'fo', did you perhaps want 'f'?
fn void foo(Foo* f)
{
  bar(f);
}

<* @param [out] completely_different_param *> // #error: There is no parameter 'completely_different_param', did you misspell it
fn void baz(Foo *f)
{
  foo2(f);
}
