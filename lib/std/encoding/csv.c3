module std::encoding::csv;
import std::io;

faultdef INCONSISTANT_ROW_LENGTH, NON_UNIQUE_ENTRY;

struct CsvReader
{
	InStream stream;
	Allocator allocator;
	CsvRow headers;
	char[] buffer;
	sz cursor;
	sz capacity;
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
	reader.buffer = alloc::new_array(allocator, char, buffer_size);
	defer catch alloc::free(allocator, reader.buffer);
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
) => self.init(tmem, stream, separator, has_header, allow_non_spec_line_endings, buffer_size,);

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

// <*
//  @param [&inout] allocator
// *>
// fn CsvRow? CsvReader.read_row(self, Allocator allocator)
// {
// 	String row = io::readline(allocator, self.stream)!;
// 	defer catch alloc::free(allocator, row);
// 	String[] list = row.split(allocator, self.separator);
// 	return { list, row, allocator };
// }

// fn CsvRow? CsvReader.tread_row(self)
// {
// 	return self.read_row(tmem) @inline;
// }

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
	int max_rows = int::max; @body(String[] row)
) @maydiscard
{
	CsvReader r;
	r.init(allocator, stream, separator, false)!;

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
			@body(parse_row_entries(self.allocator, s, self.separator, 0)!);
		};
	}
}

module std::encoding::csv @private;
import std::io, std::collections::list;

fn String? CsvReader.find_row(&self)
{
	DString row;
	row.init(self.allocator);
	defer row.free();
	sz start    = self.cursor;
	bool quoted = false;

	while (true)
	{
		if (self.cursor >= self.capacity - 1)
		{
			if (self.cursor - start) row.append_bytes(self.buffer[start:self.cursor - start]);
			sz? read = self.refill_buffer();
			start = self.cursor;
			if (catch err = read)
			{
				if (err == io::EOF)
				{
					row.append_bytes(self.buffer[start:self.capacity - start]);
					if (!row.len()) return io::EOF~;
					self.cursor++;
					break;
				}
				return err~;
			}
		}
		if (quoted && self.buffer[self.cursor:2] == "\"\"")
		{
			row.append_bytes(self.buffer[start..self.cursor]);
			self.cursor += 2;
			start = self.cursor;
		}
		if (self.buffer[self.cursor] == '"')
		{
			quoted = !quoted;
		}
		if (quoted)
		{
			self.cursor++;
			continue;
		}
		if (self.buffer[self.cursor:2] == "\r\n")
		{
			row.append_bytes(self.buffer[start..self.cursor - 1]);
			self.cursor += 2;
			break;
		}
		if (self.allow_non_spec_line_endings && self.buffer[self.cursor] == '\r' || self.buffer[self.cursor] == '\n')
		{
			row.append_bytes(self.buffer[start..self.cursor - 1]);
			self.cursor++;
			break;
		}
		self.cursor++;
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

fn sz? CsvReader.refill_buffer(&self)
{
	mem::move(self.buffer.ptr, &self.buffer[self.cursor], self.capacity - self.cursor);
	sz? read = self.stream.read(self.buffer[self.capacity - self.cursor:self.buffer.len - self.capacity + self.cursor]);
	if (catch err = read) return err~;
	self.capacity = self.capacity - self.cursor + read;
	self.cursor = 0;
	return read;
}

fn void? check_duplicate_entries(CsvRow r)
{
	for (sz i = 0; i < r.len() - 1; i++)
	{
		for (sz j = i + 1; j < r.len(); j++)
		{
			if (r[i] == r[j]) return NON_UNIQUE_ENTRY~;
		}
	}
}
