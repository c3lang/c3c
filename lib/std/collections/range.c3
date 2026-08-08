<*
 @require Type::is_ordered : "The type must be ordered"
*>
module std::collections::range <Type>;
import std::io;

struct Range (Printable)
{
	Type start;
	Type end;
}

fn sz Range.len(&self) @operator(len)
{
	if (self.end < self.start) return 0;
	return (sz)(self.end - self.start) + 1;
}

fn bool Range.contains(&self, Type value) @inline
{
	return value >= self.start && value <= self.end;
}

<*
 @require index < self.len() : "Can't index into an empty range"
*>
fn Type Range.get(&self, sz index) @operator([])
{
	return (Type)(self.start + (Type)index);
}

<*
	@require $defined(slice[0]) : "Cannot index `slice` parameter"
*>
macro Range.slice_from(self, slice)
{
	return (&slice[self.start])[:self.end - self.start + 1];
}

fn sz? Range.to_format(&self, Formatter* formatter) @dynamic
{
	return formatter.printf("[%s..%s]", self.start, self.end)!;
}

struct ExclusiveRange (Printable)
{
	Type start;
	Type end;
}

fn sz ExclusiveRange.len(&self) @operator(len)
{
	if (self.end < self.start) return 0;
	return (sz)(self.end - self.start);
}

fn bool ExclusiveRange.contains(&self, Type value) @inline
{
	return value >= self.start && value < self.end;
}

fn sz? ExclusiveRange.to_format(&self, Formatter* formatter) @dynamic
{
	return formatter.printf("[%s..<%s]", self.start, self.end)!;
}

<*
	@require $defined(slice[0]) : "Cannot index `slice` parameter"
*>
macro ExclusiveRange.slice_from(self, slice)
{
	return (&slice[self.start])[:self.end - self.start];
}

<*
 @require index < self.len() : "Can't index into an empty range"
*>
fn Type ExclusiveRange.get(&self, sz index) @operator([])
{
	return (Type)(self.start + (Type)index);
}

module std::collections::range;

<*

 @return "a Range from-to (exclusive) inferring the type."

 @require $defined(from - to, from = to, ($Typeof(from))1, from + ($Typeof(from))1)
   : "From and to values must support subtraction and addition by 1."
*>
macro inclusive(from, to)
{
	return (Range{$Typeof(from)}){ from, to };
}

<*
 Return a range starting from 0 up to a value (inclusive) `range::upto(3)` would yield 0, 1, 2, 3

 @require $defined(to - to, ($Typeof(to))0, to + ($Typeof(to))1)
   : "To value must support subtraction and addition by 1."
*>
macro upto(to)
{
	return (Range{$Typeof(to)}){ ($Typeof(to))0, to };
}

<*

 @return "an ExclusiveRange from-to (exclusive) inferring the type."

 @require $defined(from - to, from = to, ($Typeof(from))1, from + ($Typeof(from))1)
   : "From and to values must support subtraction and addition by 1."
*>
macro exclusive(from, to)
{
	return (ExclusiveRange{$Typeof(from)}){ from, to };
}
