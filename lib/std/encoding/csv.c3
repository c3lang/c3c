module std::encoding::csv;
import std::io;


struct CsvReader
{
	InStream stream;
	String separator;
}

struct CsvRow (Printable)
{
	String[] list;
	String row;
	Allocator allocator;
}

fn usz? CsvRow.to_format(&self, Formatter* f) @dynamic
{
	return f.printf("%s", self.list);
}

fn usz CsvRow.len(&self) @operator(len)
{
	return self.list.len;
}

<*
 @require col < self.list.len
*>
fn String CsvRow.get_col(&self, usz col) @operator([])
{
	return self.list[col];
}

fn void CsvReader.init(&self, InStream stream, String separator = ",")
{
	self.stream = stream;
	self.separator = separator;
}
<*
 @param [&inout] allocator
*>
fn CsvRow? CsvReader.read_row(self, Allocator allocator)
{
	String row = io::readline(allocator, self.stream)!;
	defer catch allocator::free(allocator, row);
	String[] list = row.split(allocator, self.separator);
	return { list, row, allocator };
}

fn CsvRow? CsvReader.tread_row(self)
{
	return self.read_row(tmem) @inline;
}

<*
 @require self.allocator != null  : `Row already freed`
*>
fn void CsvRow.free(&self)
{
	allocator::free(self.allocator, self.list);
	allocator::free(self.allocator, self.row);
	self.allocator = null;
}

fn void? CsvReader.skip_row(self) @maydiscard => @pool()
{
	(void)io::treadline(self.stream);
}

macro void? @each_row(InStream stream, String separator = ",", int max_rows = int.max; @body(String[] row)) @maydiscard
{
	while (max_rows--)
	{
		@stack_mem(512; mem)
		{
			String? s = io::readline(mem, stream);
			if (catch err = s)
			{
				if (err == io::EOF) return;
				return err?;
			}
			@body(s.split(mem, separator));
		};
	}
}

macro void? CsvReader.@each_row(self, int rows = int.max; @body(String[] row)) @maydiscard
{
	return @each_row(self.stream, self.separator, rows; row)
	{
		@body(row);
	};
}