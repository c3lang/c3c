module std::encoding::csv;
import std::io;

struct CsvReader
{
	Stream stream;
	String separator;
}

fn void CsvReader.init_file(CsvReader* csv, File* file, String separator = ",")
{
	csv.stream = file.as_stream();
	csv.separator = separator;
}

fn void CsvReader.init(CsvReader* csv, Stream stream, String separator = ",")
{
	csv.stream = stream;
	csv.separator = separator;
}

fn String[]! CsvReader.read_row(CsvReader csv, Allocator* using = mem::heap())
{
	@stack_mem(512; Allocator* mem)
	{
		return csv.stream.readline(mem).split(csv.separator, .using = using);
	};
}

fn String[]! CsvReader.tread_row(CsvReader csv)
{
	return csv.read_row(mem::temp()) @inline;
}

fn void! CsvReader.skip_row(CsvReader csv) @maydiscard
{
	@pool()
	{
		csv.stream.readline(mem::temp())!;
	};
}

macro CsvReader.@each_row(CsvReader csv, int rows = int.max; @body(String[] row))
{
	Stream stream = csv.stream;
	String sep = csv.separator;
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