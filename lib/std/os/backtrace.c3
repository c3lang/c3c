module std::os::backtrace;

fault BacktraceFault
{
	SEGMENT_NOT_FOUND,
	EXECUTABLE_PATH_NOT_FOUND,
	IMAGE_NOT_FOUND,
	NO_BACKTRACE_SYMBOLS,
}

const Backtrace BACKTRACE_UNKNOWN = { 0, "", "", "", 0, null };

struct Backtrace
{
	uptr offset;
	String function;
	String object_file;
	String file;
	uint line;
	Allocator* allocator;
}

fn bool Backtrace.has_file(&self)
{
	return self.file.len > 0;
}

fn bool Backtrace.is_unknown(&self)
{
	return !self.object_file.len;
}

fn usz! Backtrace.to_format(&self, Formatter* formatter) : Printable
{
	if (self.has_file())
	{
		return formatter.printf("%s (in %s) (%s:%d)", self.function, self.object_file, self.file, self.line);
	}
	if (self.is_unknown())
	{
		return formatter.printf("??? (in unknown)");
	}
	return formatter.printf("%s (in %s) (source unavailable)", self.function, self.object_file);
}
fn void Backtrace.free(&self)
{
	if (!self.allocator) return;
	free(self.function, .using = self.allocator);
	free(self.object_file, .using = self.allocator);
	free(self.file, .using = self.allocator);
}

fn Backtrace* Backtrace.init(&self, uptr offset, String function, String object_file, String file = "", uint line = 0, Allocator* using = mem::heap())
{
	if (!using)
	{
		self.offset = offset;
		self.function = function;
		self.object_file = object_file;
		self.file = file;
		self.line = 0;
		self.allocator = null;
		return self;
	}
	self.offset = offset;
	self.function = function.copy(using);
	self.object_file = object_file.copy(using);
	self.file = file.copy(using);
	self.allocator = using;
	self.line = line;
	return self;
}

