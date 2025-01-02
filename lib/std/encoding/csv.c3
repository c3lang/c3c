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

fn usz! CsvRow.to_format(&self, Formatter* f) @dynamic
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

fn CsvRow! CsvReader.read_new_row(self)
{
	return self.read_row(allocator::heap()) @inline;
}

<*
 @param [&inout] allocator
*>
fn CsvRow! CsvReader.read_row(self, Allocator allocator)
{
	String row = io::readline(self.stream, allocator: allocator)!;
	defer catch allocator::free(allocator, row);
	String[] list = row.split(self.separator, allocator: allocator);
	return { list, row, allocator };
}

fn CsvRow! CsvReader.read_temp_row(self)
{
	return self.read_row(allocator::temp()) @inline;
}

<*
 @require self.allocator `Row already freed`
*>
fn void CsvRow.free(&self)
{
	allocator::free(self.allocator, self.list);
	allocator::free(self.allocator, self.row);
	self.allocator = null;
}

fn void! CsvReader.skip_row(self) @maydiscard
{
	@pool()
	{
		(void)io::treadline(self.stream);
	};
}

macro void! CsvReader.@each_row(self, int rows = int.max; @body(String[] row)) @maydiscard
{
	InStream stream = self.stream;
	String sep = self.separator;
	while (rows--)
	{
		@stack_mem(512; Allocator mem)
		{
			String! s = io::readline(stream, mem);
			if (catch err = s)
			{
				if (err == IoError.EOF) return;
				return err?;
			}
			@body(s.split(sep, allocator: mem));
		};
	}
}