module std::collections::enummap(<Enum, ValueType>);

struct EnumMap
{
	ValueType[Enum.len] values;
}

fn void EnumMap.init(&self, ValueType init_value)
{
	foreach(&a : self.values)
	{
		*a = init_value;
	}
}


fn void! EnumMap.to_format(&self, Formatter* formatter) @dynamic
{
	formatter.print("{ ")!;
	foreach (i, &value : self.values)
	{
		if (i != 0) formatter.print(", ")!;
		formatter.printf("%s: %s", (Enum)i, *value)!;
	}
	formatter.print(" }")!;
}

fn String EnumMap.to_string(&self, Allocator* using = mem::heap()) @dynamic
{
	return string::printf("%s", *self);
}

fn uint EnumMap.len(&self) @operator(len) => self.values.len;
fn ValueType EnumMap.get(&self, Enum key) @operator([]) => self.values[key.ordinal];
fn void EnumMap.set(&self, Enum key, ValueType value) @operator([]=) => self.values[key.ordinal] = value;
