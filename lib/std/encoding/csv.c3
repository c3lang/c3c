module std::encoding::csv;
import std::io;

faultdef INCONSISTANT_ROW_LENGTH, NON_UNIQUE_ENTRY, INCORRECT_LINE_ENDING;

struct CsvReader
{
	InStream stream;
	Allocator allocator;
	CsvRow headers;
	String delimeter;
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
	String delimeter = ",",
	bool has_header = true,
	bool allow_non_spec_line_endings = false,
) => load(tmem, stream, delimeter, has_header, allow_non_spec_line_endings);

fn CsvReader? load(
	Allocator allocator,
	InStream stream,
	String delimeter = ",",
	bool has_header = true,
	bool allow_non_spec_line_endings = false,
)
{
	CsvReader reader;
	reader.allocator = allocator;
	reader.stream = stream;
	reader.delimeter = delimeter;
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
	String delimeter = ",",
	bool has_header = true,
	bool allow_non_spec_line_endings = false,
) => self.init(
	tmem,
	stream,
	delimeter,
	has_header,
	allow_non_spec_line_endings,
);

fn void? CsvReader.init(
	&self,
	Allocator allocator,
	InStream stream,
	String delimeter = ",",
	bool has_header = true,
	bool allow_non_spec_line_endings = false,
)
{
	*self = load(allocator, stream, delimeter, has_header, allow_non_spec_line_endings)!;
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

<*
 @require self.allocator != null : `CsvReader is not initialized`
*>
fn void? CsvReader.skip_row(self) @maydiscard => @pool()
{
	(void)self.find_row()!;
}

<*
 @require self.allocator != null : `CsvReader is not initialized`
*>
fn CsvRow? CsvReader.read_row(&self)
{
	String row = self.find_row()!;

	return { parse_row_entries(self.allocator, row, self.delimeter, self.headers.len()), row, self.allocator };
}

macro void? @each_row(
	Allocator allocator,
	InStream stream,
	String delimeter = ",",
	bool allow_non_spec_line_endings,
	int max_rows = int::max; @body(String[] row)
) @maydiscard
{
	CsvReader r;
	r.init(allocator, stream, delimeter, false, allow_non_spec_line_endings)!;
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
			String[] row = parse_row_entries(self.allocator, s, self.delimeter, 0)!;
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
	ENDING,
	QUOTE,
	STRING,
	NORMAL,
}

fn String? CsvReader.find_row(&self)
{
	DString row;
	row.init(self.allocator, 256);
	defer row.free();
	ReadState state = NORMAL;
	char? current;
	bool ending     = false;

	while (!ending)
	{
		current = self.stream.read_byte();
		if (catch err = current)
		{
			if (err == io::EOF)
			{
				if (!row.len()) return io::EOF~;
				if (state == QUOTE) row.append_char('"');
				if (state == STRING) return encoding::MALFORMED_DATA~;
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
				if (state == STRING)
				{
					row.append("\\\"");
					break;
				}
				row.append_char('"');
				if (current == '\r' || current == '\n') nextcase ENDING;
				row.append_char(current);
			case STRING:
				if (current == '"')
				{
					state = QUOTE;
					break;
				}
				row.append_char(current);
			case NORMAL:
				if (current == '\r' || current == '\n') nextcase ENDING;
				if (current == '"') state = STRING;
				row.append_char(current);
		}
	}
	return row.copy_str(self.allocator);
}

enum ParseState
{
	STRING,
	NORMAL,
	PUSH,
}

fn String[]? parse_row_entries(Allocator allocator, String row, String delimeter, sz entry_amount = 0)
{
	if (!row.len) return { };
	sz capacity      = entry_amount ? entry_amount : 16;
	String* holder   = alloc::alloc_array(allocator, String, capacity);
	sz entry_index   = 0;
	sz row_index     = 0;
	sz reading_index = 0;
	ParseState state = NORMAL;
	char current;

	while (true)
	{
		if (row_index + reading_index == row.len)
		{
			if (entry_index == capacity)
			{
				capacity *= 2;
				holder = alloc::realloc(allocator, holder, String::size * capacity);
			}
			holder[entry_index++] = row[row_index:reading_index];
			break;
		}
		current = row[row_index + reading_index];

		switch (state)
		{
			case STRING:
				if (current == '\\')
				{
					reading_index += 2;
					continue;
				}
				if (current == '"')
				{
					state = NORMAL;
				}
				reading_index++;
			case NORMAL:
				if (current == delimeter[0] && row[row_index + reading_index:delimeter.len] == delimeter)
				{
					nextcase PUSH;
				}
				if (current == '"')
				{
					state = STRING;
				}
				reading_index++;
			case PUSH:
				if (entry_index == capacity)
				{
					capacity *= 2;
					holder = alloc::realloc(allocator, holder, String::size * capacity);
				}
				holder[entry_index++] = row[row_index:reading_index];
				row_index += reading_index + delimeter.len;
				reading_index = 0;
		}
	}
	if (entry_amount && entry_index != entry_amount) return INCONSISTANT_ROW_LENGTH~;

	return holder[:entry_index];
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
