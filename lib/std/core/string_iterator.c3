module std::core::string::iterator;

struct StringIterator
{
	String utf8;
	sz current;
}

fn void StringIterator.reset(&self)
{
	self.current = 0;
}

fn Char32? StringIterator.next(&self)
{
    sz len = self.utf8.len;
    sz current = self.current;
    if (current >= len) return NO_MORE_ELEMENT~;
    sz read = (len - current < 4 ? len - current : 4);
    Char32 res = conv::utf8_to_char32(&self.utf8[current], &read)!;
    self.current += read;
    return res;
}

fn Char32? StringIterator.peek(&self)
{
    sz len = self.utf8.len;
    sz current = self.current;
    if (current >= len) return NO_MORE_ELEMENT~;
    sz read = (len - current < 4 ? len - current : 4);
    Char32 res = conv::utf8_to_char32(&self.utf8[current], &read)!;
    return res;
}

fn bool StringIterator.has_next(&self)
{
    return self.current < self.utf8.len;
}

fn Char32? StringIterator.get(&self)
{
    sz len = self.utf8.len;
    sz current = self.current;
    sz read = (len - current < 4 ? len - current : 4);
    sz index = current > read ? current - read : 0;
    if (index >= len) return NO_MORE_ELEMENT~;
    Char32 res = conv::utf8_to_char32(&self.utf8[index], &read)!;
    return res;
}
