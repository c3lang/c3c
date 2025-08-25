module function_tag;

macro bool @has_tagof_1(#function)
{
  return #function.has_tagof("some-tag");
}

fn void tagged_function() @tag("some-tag", true) {}

fn void function_tag() @test
{
  assert(@has_tagof_1(tagged_function));
  assert(@has_tagof_1(fn void() @tag("some-tag", true) {} ));
}
