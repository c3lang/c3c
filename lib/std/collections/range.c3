/**
 * @require Type.is_ordered : "The type must be ordered"
 **/
module std::collections::range(<Type>);

struct Range (Printable)
{
	Type start;
	Type end;
}

fn usz Range.len(&self) @operator(len)
{
	if (self.end < self.start) return 0;
	return (usz)(self.end - self.start) + 1;
}

fn bool Range.contains(&self, Type value) @inline
{
	return value >= self.start && value <= self.end;
}

/**
 * @require index < self.len() : "Can't index into an empty range"
 **/
fn Type Range.get(&self, usz index) @operator([])
{
	return (Type)(self.start + (usz)index);
}

fn String Range.to_new_string(&self, Allocator* allocator = allocator::heap()) @dynamic
{
	return string::new_format("[%s..%s]", self.start, self.end, .allocator = allocator);
}

fn String Range.to_tstring(&self)
{
	return self.to_new_string(allocator::temp());
}

fn usz! Range.to_format(&self, Formatter* formatter) @dynamic
{
	return formatter.printf("[%s..%s]", self.start, self.end)!;
}

struct ExclusiveRange (Printable)
{
	Type start;
	Type end;
}

fn usz ExclusiveRange.len(&self) @operator(len)
{
	if (self.end < self.start) return 0;
	return (usz)(self.end - self.start);
}

fn bool ExclusiveRange.contains(&self, Type value) @inline
{
	return value >= self.start && value < self.end;
}

fn usz! ExclusiveRange.to_format(&self, Formatter* formatter) @dynamic
{
	return formatter.printf("[%s..<%s]", self.start, self.end)!;
}

fn String ExclusiveRange.to_new_string(&self, Allocator* allocator = allocator::heap()) @dynamic
{
	return string::new_format("[%s..<%s]", self.start, self.end, .allocator = allocator);
}

fn String ExclusiveRange.to_tstring(&self)
{
	return self.to_new_string(allocator::temp());
}

/**
 * @require index < self.len() : "Can't index into an empty range"
 **/
fn Type ExclusiveRange.get(&self, usz index) @operator([])
{
	return (Type)(self.start + index);
}
