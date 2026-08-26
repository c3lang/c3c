module std::encoding::csv;
import std::io;

faultdef INCONSISTANT_ROW_LENGTH, NON_UNIQUE_ENTRY, INCORRECT_LINE_ENDING;

struct CsvReader
{
	InStream stream;
	Allocator allocator;
	CsvRow headers;
	String separator;
	bitstruct : char
	{
		bool has_header;
		bool allow_non_spec_line_endings;
	}
}

struct CsvRow (Printable)
{
	String[] list;
	String row;
	Allocator allocator;
}

fn CsvReader? tload(
	InStream stream,
	String separator = ",",
	bool has_header = true,
	bool allow_non_spec_line_endings = false,
	sz buffer_size = 256
) => load(tmem, stream, separator, has_header, allow_non_spec_line_endings, buffer_size);

fn CsvReader? load(
	Allocator allocator,
	InStream stream,
	String separator = ",",
	bool has_header = true,
	bool allow_non_spec_line_endings = false,
	sz buffer_size = 256
)
{
	CsvReader reader;
	reader.allocator = allocator;
	reader.stream = stream;
	reader.separator = separator;
	reader.has_header = has_header;
	reader.allow_non_spec_line_endings = allow_non_spec_line_endings;
	if (has_header)
	{
		reader.headers = reader.read_row()!;
		check_duplicate_entries(reader.headers)!;
	}
	return reader;
}

fn void? CsvReader.tinit(
	&self,
	InStream stream,
	String separator = ",",
	bool has_header = true,
	bool allow_non_spec_line_endings = false,
	sz buffer_size = 256
) => self.init(
	tmem,
	stream,
	separator,
	has_header,
	allow_non_spec_line_endings,
	buffer_size,
);

fn void? CsvReader.init(
	&self,
	Allocator allocator,
	InStream stream,
	String separator = ",",
	bool has_header = true,
	bool allow_non_spec_line_endings = false,
	sz buffer_size = 256
)
{
	*self = load(allocator, stream, separator, has_header, allow_non_spec_line_endings, buffer_size)!;
}

fn void CsvReader.free(&self)
{
	if (self.headers.len()) self.headers.free();
	self.allocator = null;
}

fn sz? CsvRow.to_format(&self, Formatter* f) @dynamic
{
	return f.printf("%s", self.list);
}

fn sz CsvRow.len(&self) @operator(len)
{
	return self.list.len;
}

<*
 @require col < self.list.len
*>
fn String CsvRow.get_col(&self, sz col) @operator([])
{
	return self.list[col];
}

<*
 @require self.allocator != null : `Row already freed`
*>
fn void CsvRow.free(&self)
{
	alloc::free(self.allocator, self.list);
	alloc::free(self.allocator, self.row);
	self.allocator = null;
}

fn void? CsvReader.skip_row(self) @maydiscard => @pool()
{
	(void)self.find_row()!;
}

fn CsvRow? CsvReader.read_row(&self)
{
	String row = self.find_row()!;

	return { parse_row_entries(self.allocator, row, self.separator, self.headers.len()), row, self.allocator };
}

macro void? @each_row(
	Allocator allocator,
	InStream stream,
	String separator = ",",
	bool allow_non_spec_line_endings,
	int max_rows = int::max; @body(String[] row)
) @maydiscard
{
	CsvReader r;
	r.init(allocator, stream, separator, false, allow_non_spec_line_endings)!;
	defer r.free();

	return r.@each_row(max_rows ; row)
	{
		@body(row);
	};
}

macro void? CsvReader.@each_row(&self, int max_rows = int::max; @body(String[] row)) @maydiscard
{
	while (max_rows--)
	{
		@stack_mem(512 ; mem)
		{
			String? s = self.find_row();
			if (catch err = s)
			{
				if (err == io::EOF) return;
				return err~;
			}
			String[] row = parse_row_entries(self.allocator, s, self.separator, 0)!;
			@body(row);
			alloc::free(self.allocator, row);
			alloc::free(self.allocator, s);
		};
	}
}

module std::encoding::csv @private;
import std::io, std::collections::list;

enum ReadState
{
	NORMAL,
	STRING,
	QUOTE,
	ENDING,
}

fn String? CsvReader.find_row(&self)
{
	DString row;
	row.init(self.allocator);
	defer row.free();
	ReadState state = NORMAL;
	char? current;
	bool ending     = false;

	while (!ending)
	{
		current = self.stream.read_byte();
		if (catch err = current)
		{
			if (err == io::EOF) {
				if (!row.len()) return io::EOF~;
				break;
			}
			return err~;
		}
		// Handle CRLF line ending when allowing non spec line endings
		if (!row.len() && self.allow_non_spec_line_endings && current == '\n') continue;
		
		switch (state)
		{
			case ENDING:
				if (self.allow_non_spec_line_endings)
				{
					ending = true;
					break;
				}
				if (current == '\n')
				{
					row.append_char(current);
				}
				else
				{
					char? next = self.stream.read_byte();
					if (catch err = next)
					{
						if (err == io::EOF)
						{
							row.append_char(current);
							break;
						}
						return err~;
					}
					if (next == '\n')
					{
						ending = true;
						break;
					}
					else
					{
						row.append_char(current);
						current = next;
						nextcase NORMAL;
					}
				}
			case QUOTE:
				state = current == '"' ? STRING : NORMAL;
				nextcase state;
			case STRING:
				if (current == '"') state = QUOTE;
				row.append_char(current);
			case NORMAL:
				if (current == '\r' || current == '\n') nextcase ENDING;
				if (current == '"') state = STRING;
				row.append_char(current);
		}
	}
	return row.copy_str(self.allocator);
}

fn String[]? parse_row_entries(Allocator allocator, String s, String separator, sz entry_count = 0)
{
	if (!s.len) return { };
	sz capacity    = entry_count ? entry_count : 16;
	String* holder = alloc::alloc_array(allocator, String, capacity);
	sz i           = 0;
	bool going     = true;

	while (going)
	{
		sz index = min(s.index_of("\"") ?? (sz::max - 1), s.index_of(separator) ?? (sz::max - 1));
		String res @noinit;
		if (index >= s.len)
		{
			res = s;
			going = false;
		}
		else
		{
			if (s[index] == '"')
			{
				while (true)
				{
					sz? quoted = 1 + s[index + 1..].index_of_char('"');
					if (catch err = quoted)
					{
						return encoding::MALFORMED_DATA~;
					}
					else if (index + quoted == s.len - 1 || s[index + quoted + 1:separator.len] == separator)
					{
						index += quoted + 1;
						break;
					}
					index += quoted;
				}
			}
			res = s[:index];
			if (index >= s.len)
			{
				going = false;
			}
			else
			{
				s = s[index + separator.len..];
			}
		}
		if (i == capacity)
		{
			capacity *= 2;
			holder = alloc::realloc(allocator, holder, String::size * capacity);
		}
		holder[i++] = res;
	}
	if (entry_count && i != entry_count) return INCONSISTANT_ROW_LENGTH~;

	return holder[:i];
}

fn void? check_duplicate_entries(CsvRow row)
{
	for (sz i = 0; i < row.len() - 1; i++)
	{
		for (sz j = i + 1; j < row.len(); j++)
		{
			if (row[i] == row[j]) return NON_UNIQUE_ENTRY~;
		}
	}
}
