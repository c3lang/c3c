module std::os::backtrace;
import std::collections::list, std::os, std::io;

faultdef SEGMENT_NOT_FOUND, EXECUTABLE_PATH_NOT_FOUND, IMAGE_NOT_FOUND, NO_BACKTRACE_SYMBOLS,
         RESOLUTION_FAILED;

const Backtrace BACKTRACE_UNKNOWN = { 0, "", "", "", 0, null, false };

struct Backtrace (Printable)
{
	uptr offset;
	String function;
	String object_file;
	String file;
	uint line;
	Allocator allocator;
	bool is_inline;
}


fn bool Backtrace.has_file(&self)
{
	return self.file.len > 0;
}

fn bool Backtrace.is_unknown(&self)
{
	return !self.object_file.len;
}

fn usz? Backtrace.to_format(&self, Formatter* formatter) @dynamic
{
	String inline_suffix = self.is_inline ? " [inline]" : "";
	if (self.has_file())
	{
		return formatter.printf("%s (in %s) (%s:%d)%s", self.function, self.object_file, self.file, self.line, inline_suffix);
	}
	if (self.is_unknown())
	{
		return formatter.printf("??? (in unknown)%s", inline_suffix);
	}
	return formatter.printf("%s (in %s) (source unavailable)%s", self.function, self.object_file, inline_suffix);
}
fn void Backtrace.free(&self)
{
	if (!self.allocator) return;
	allocator::free(self.allocator, self.function);
	allocator::free(self.allocator, self.object_file);
	allocator::free(self.allocator, self.file);
}

fn Backtrace* Backtrace.init(&self, Allocator allocator, uptr offset, String function, String object_file, String file = "", uint line = 0)
{
	if (!allocator)
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
	self.function = function.copy(allocator);
	self.object_file = object_file.copy(allocator);
	self.file = file.copy(allocator);
	self.allocator = allocator;
	self.line = line;
	return self;
}

fn void*[] capture_current(void*[] buffer)
{
	if (!buffer.len) return buffer[:0];
	$switch:
		$case env::POSIX:
			CInt len = posix::backtrace(buffer.ptr, buffer.len);
			return buffer[:len];
		$case env::WIN32:
			Win32_WORD len = win32::rtlCaptureStackBackTrace(0, buffer.len, buffer.ptr, null);
			return buffer[:len];
		$default:
			return buffer[:0];
	$endswitch
}

alias BacktraceList = List{Backtrace};

alias symbolize_backtrace @if(env::LINUX)  = linux::symbolize_backtrace;
alias symbolize_backtrace @if(env::WIN32)  = win32::symbolize_backtrace;
alias symbolize_backtrace @if(env::DARWIN) = darwin::symbolize_backtrace;

fn BacktraceList? symbolize_backtrace(Allocator allocator, void*[] backtrace) @if(!env::NATIVE_STACKTRACE)
{
	return {};
}