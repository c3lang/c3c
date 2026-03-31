<*
 @require Type.is_ordered : "The type must be ordered"
*>
module std::collections::range <Type>;
import std::io;

struct Range (Printable)
{
	Type start;
	Type end;
}

fn isz Range.len(&self) @operator(len)
{
	if (self.end < self.start) return 0;
	return (isz)(self.end - self.start) + 1;
}

fn bool Range.contains(&self, Type value) @inline
{
	return value >= self.start && value <= self.end;
}

<*
 @require index < self.len() : "Can't index into an empty range"
*>
fn Type Range.get(&self, isz index) @operator([])
{
	return (Type)(self.start + (isz)index);
}

fn isz? Range.to_format(&self, Formatter* formatter) @dynamic
{
	return formatter.printf("[%s..%s]", self.start, self.end)!;
}

struct ExclusiveRange (Printable)
{
	Type start;
	Type end;
}

fn isz ExclusiveRange.len(&self) @operator(len)
{
	if (self.end < self.start) return 0;
	return (isz)(self.end - self.start);
}

fn bool ExclusiveRange.contains(&self, Type value) @inline
{
	return value >= self.start && value < self.end;
}

fn isz? ExclusiveRange.to_format(&self, Formatter* formatter) @dynamic
{
	return formatter.printf("[%s..<%s]", self.start, self.end)!;
}

<*
 @require index < self.len() : "Can't index into an empty range"
*>
fn Type ExclusiveRange.get(&self, isz index) @operator([])
{
	return (Type)(self.start + index);
}
