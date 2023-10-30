module std::collections::maybe(<Type>);

struct Maybe
{
	Type value;
	bool has_value;
}

fn Maybe value(Type val)
{
	return { .value = val, .has_value = true };
}

const Maybe EMPTY = { };

macro Type! Maybe.get(self)
{
	return self.has_value ? self.value : SearchResult.MISSING?;
}
