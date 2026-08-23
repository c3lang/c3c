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

fn Type[] Range.to_array(&self, Allocator allocator)
{
	sz len = self.len();
	if (!len) return {};
	Type[] result = alloc::alloc_array(allocator, Type, len);
	foreach (i, value : self) result[i] = value;
	return result;
}


fn sz? Range.to_format(&self, Formatter* formatter) @dynamic
{
	return formatter.printf("[%s..%s]", self.start, self.end)!;
}

<* Check that the range is not negative *>
macro bool Range.is_valid(self) => self.end + 1 >= self.start;

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
 @require index < self.len() : "Can't index into an empty range"
*>
fn Type ExclusiveRange.get(&self, sz index) @operator([])
{
	return (Type)(self.start + (Type)index);
}

fn Type[] ExclusiveRange.to_array(&self, Allocator allocator)
{
	sz len = self.len();
	if (!len) return {};
	Type[] result = alloc::alloc_array(allocator, Type, len);
	foreach (i, value : self) result[i] = value;
	return result;
}

<* Check that the range is not negative *>
macro bool ExclusiveRange.is_valid(self) => self.end >= self.start;

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

<*
 @require $defined(&array[0], array.len) : "Cannot index `array` parameter"
 @require $Typeof(range)::is_generic(Range) ||| $Typeof(range)::is_generic(ExclusiveRange) : "`range` parameter was not of type `Range` or `ExclusiveRange`."
 @require range.start >= 0 && range.end <= array.len : "Range out of bounds"
 @require !$Typeof(range)::is_generic(Range) ||| range.end < array.len : "Range is out of bounds"
 @require range.is_valid() : "Range is reversed"
*>
macro slice(array, range)
{
	$if $Typeof(range)::is_generic(Range):
		return (&array[range.start])[:range.end - range.start + 1];
	$else
		return (&array[range.start])[:range.end - range.start];
	$endif
}
