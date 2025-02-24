module std::core::test::string_iterator::tests @test;

fn void test_at_start()
{
    String test = "abcd";
    StringIterator iterator = test.iterator();
    assert(iterator.get()!! == 'a');
    assert(iterator.peek()!! == 'a');
    iterator.next()!!;
    assert(iterator.next()!! == 'b');
    assert(iterator.has_next());
}


fn void test_general()
{ 
    String test = "åƦs1";
    StringIterator iterator = test.iterator();
    assert(iterator.get()!! == 'å');
    iterator.next()!!;
    assert(iterator.peek()!! == 'Ʀ');
    assert(iterator.next()!! == 'Ʀ');
    iterator.reset();
    assert(iterator.current == 0);
}

fn void test_end()
{
    String test = "åƦ";
    StringIterator iterator = test.iterator();
    assert(@ok(iterator.next()));
    assert(iterator.peek()!! == 'Ʀ');
    assert(@ok(iterator.next()));
    assert(@catch(iterator.next()));
}
