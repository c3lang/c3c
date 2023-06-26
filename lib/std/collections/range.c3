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

/**
 * @require index < range.len() : "Can't index into an empty range"
 **/
fn Type ExclusiveRange.get(ExclusiveRange* range, usz index) @operator([])
{
    return range.start + (Type)index;
}
