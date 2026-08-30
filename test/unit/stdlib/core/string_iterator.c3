module std::core::test::string_iterator::tests @test;

fn void test_at_start()
{
    String test = "abcd";
    StringIterator iterator = test.iterator();
    assert(iterator.peek()!! == 'a');
    iterator.next()!!;
    assert(iterator.next()!! == 'b');
    assert(iterator.has_next());
}


fn void test_general()
{ 
    String test = "åƦs1";
    StringIterator iterator = test.iterator();
    assert(iterator.next()!! == 'å');
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
    assert(!iterator.has_next());
    assert(@catch(iterator.peek()) == NO_MORE_ELEMENT);
    assert(@catch(iterator.next()) == NO_MORE_ELEMENT);
}

fn void test_three_and_four_byte_codepoints()
{
    // "€" is 3 bytes (U+20AC), "😀" is 4 bytes (U+1F600)
    String test = "€😀x";
    StringIterator iterator = test.iterator();
    assert(iterator.has_next());
    assert(iterator.next()!! == '€');
    assert(iterator.peek()!! == '😀');
    assert(iterator.next()!! == '😀');
    assert(iterator.next()!! == 'x');
    assert(!iterator.has_next());
}

fn void test_invalid_utf8()
{
    // Lone continuation byte is not a valid start of a UTF-8 sequence.
    String test = (String)((char[]){ 0x80, 'a' });
    StringIterator iterator = test.iterator();
    assert(@catch(iterator.next()) == string::INVALID_UTF8);
    assert(@catch(iterator.peek()) == string::INVALID_UTF8);
}

fn void test_truncated_utf8()
{
    // A 3-byte sequence lead byte with only one continuation byte available.
    String test = (String)((char[]){ 0xE2, 0x82 });
    StringIterator iterator = test.iterator();
    assert(@catch(iterator.next()) == string::INVALID_UTF8);
}
