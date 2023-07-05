/**
 * @checked Type{} < Type{} : "The type must be comparable"
 * @checked Type{} + (Type)1 : "The type must be possible to add to"
 **/
module std::collections::range(<Type>);

struct Range
{
	Type start;
	Type end;
}

fn usz Range.len(&self) @operator(len)
{
	if (self.end < self.start) return 0;
	return (usz)(self.end - self.start + (Type)1);
}

/**
 * @require index < self.len() : "Can't index into an empty range"
 **/
fn Type Range.get(&self, usz index) @operator([])
{
    return self.start + (Type)index;
}

fn String Range.to_string(&self, Allocator* using = mem::heap()) @dynamic
{
	return string::printf("[%s..%s]", self.start, self.end);
}

fn void! Range.to_format(&self, Formatter* formatter) @dynamic
{
	formatter.printf("[%s..%s]", self.start, self.end)!;
}

struct ExclusiveRange
{
	Type start;
	Type end;
}

fn usz ExclusiveRange.len(&self) @operator(len)
{
	if (self.end < self.start) return 0;
	return (usz)(self.end - self.start);
}

fn void! ExclusiveRange.to_format(&self, Formatter* formatter) @dynamic
{
	formatter.printf("[%s..<%s]", self.start, self.end)!;
}

fn String ExclusiveRange.to_string(&self, Allocator* using = mem::heap()) @dynamic
{
	return string::printf("[%s..<%s]", self.start, self.end);
}

/**
 * @require index < self.len() : "Can't index into an empty range"
 **/
fn Type ExclusiveRange.get(&self, usz index) @operator([])
{
    return self.start + (Type)index;
}
