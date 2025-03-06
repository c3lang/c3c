module std::collections::maybe{Type};
import std::io;

struct Maybe (Printable)
{
	Type value;
	bool has_value;
}

fn usz? Maybe.to_format(&self, Formatter* f) @dynamic
{
	if (self.has_value) return f.printf("[%s]", self.value);
	return f.printf("[EMPTY]");
}

fn void Maybe.set(&self, Type val)
{
	*self = { .value = val, .has_value = true };
}

fn void Maybe.reset(&self)
{
	*self = {};
}

fn Maybe value(Type val)
{
	return { .value = val, .has_value = true };
}

const Maybe EMPTY = { };

macro Type? Maybe.get(self)
{
	return self.has_value ? self.value : NOT_FOUND?;
}
