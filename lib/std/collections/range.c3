/**
 * @checked Type{} < Type{} : "The type must be comparable"
 * @checked Type{} + (Type)1 : "The type must be possible to add to"
 **/
module std::collections::range<Type>;

struct Range
{
	Type start;
	Type end;
}

fn usz Range.len(Range* range) @operator(len)
{
	if (range.end < range.start) return 0;
	return (usz)(range.end - range.start + (Type)1);
}

/**
 * @require index < range.len() : "Can't index into an empty range"
 **/
fn Type Range.get(Range* range, usz index) @operator([])
{
    return range.start + (Type)index;
}

fn String Range.to_string(Range* list, Allocator* using = mem::heap()) @dynamic
{
	return string::printf("[%s..%s]", list.start, list.end);
}

fn void! Range.to_format(Range* list, Formatter* formatter) @dynamic
{
	formatter.printf("[%s..%s]", list.start, list.end)!;
}

struct ExclusiveRange
{
	Type start;
	Type end;
}

fn usz ExclusiveRange.len(ExclusiveRange* range) @operator(len)
{
	if (range.end < range.start) return 0;
	return (usz)(range.end - range.start);
}

fn void! ExclusiveRange.to_format(ExclusiveRange* list, Formatter* formatter) @dynamic
{
	formatter.printf("[%s..<%s]", list.start, list.end)!;
}

fn String ExclusiveRange.to_string(ExclusiveRange* list, Allocator* using = mem::heap()) @dynamic
{
	return string::printf("[%s..<%s]", list.start, list.end);
}

/**
 * @require index < range.len() : "Can't index into an empty range"
 **/
fn Type ExclusiveRange.get(ExclusiveRange* range, usz index) @operator([])
{
    return range.start + (Type)index;
}
