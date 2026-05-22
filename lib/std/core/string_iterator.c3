module std::core::string::iterator;
import std::math;

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
	sz read = math::min(len - current, (sz)4);
	Char32 res = conv::utf8_to_char32(&self.utf8[current], &read)!;
	self.current += read;
	return res;
}

fn Char32? StringIterator.peek(&self)
{
	sz len = self.utf8.len;
	sz current = self.current;
	if (current >= len) return NO_MORE_ELEMENT~;
	sz read = math::min(len - current, (sz)4);
	Char32 res = conv::utf8_to_char32(&self.utf8[current], &read)!;
	return res;
}

fn bool StringIterator.has_next(&self)
{
	return self.current < self.utf8.len;
}
