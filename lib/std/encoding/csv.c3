module std::encoding::csv;
import std::io;

struct CsvReader
{
	InStream* stream;
	String separator;
}

fn void CsvReader.init(&self, InStream* stream, String separator = ",")
{
	self.stream = stream;
	self.separator = separator;
}

fn String[]! CsvReader.read_row(self, Allocator* using = mem::heap())
{
	@pool(using)
	{
		return io::treadline(self.stream).split(self.separator, .using = using);
	};
}

fn String[]! CsvReader.tread_row(self)
{
	return self.read_row(mem::temp()) @inline;
}

fn void! CsvReader.skip_row(self) @maydiscard
{
	@pool()
	{
		(void)io::treadline(self.stream);
	};
}

macro CsvReader.@each_row(self, int rows = int.max; @body(String[] row))
{
	InputStream* stream = self.stream;
	String sep = self.separator;
	while (rows--)
	{
		@stack_mem(512; Allocator* mem)
		{
			String[] parts;
			@pool()
			{
				String! s = stream.readline(mem::temp());
				if (catch err = s)
				{
					if (err == IoError.EOF) return;
					return err?;
				}
				parts = s.split(sep, .using = mem);
			};
			@body(parts);
		};
	}
}