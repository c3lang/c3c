module std::encoding::csv;
import std::io;

faultdef INCONSISTANT_ROW_LENGTH, NON_UNIQUE_ENTRY;

struct CsvReader
{
	InStream stream;
	Allocator allocator;
	String[] headers;
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
	sz buffer_size = 256,
	bool has_header = true,
	bool allow_non_spec_line_endings = false
) => load(tmem, stream, separator, buffer_size, has_header, allow_non_spec_line_endings);

fn CsvReader? load(
	Allocator allocator,
	InStream stream,
	String separator = ",",
	sz buffer_size = 256,
	bool has_header = true,
	bool allow_non_spec_line_endings = false
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
		reader.headers = reader.parse_row()!.list;
		for (sz i = 0; i < reader.headers.len - 1; i++)
		{
			for (sz j = i + 1; j < reader.headers.len; j++)
			{
				if (reader.headers[i] == reader.headers[j]) return NON_UNIQUE_ENTRY~;
			}
		}
	}
	return reader;
}

fn void? CsvReader.tinit(
	&self,
	InStream stream,
	String separator = ",",
	sz buffer_size = 256,
	bool has_header = true,
	bool allow_non_spec_line_endings = false
) => self.init(tmem, stream, separator, buffer_size, has_header, allow_non_spec_line_endings);

fn void? CsvReader.init(
	&self,
	Allocator allocator,
	InStream stream,
	String separator = ",",
	sz buffer_size = 256,
	bool has_header = true,
	bool allow_non_spec_line_endings = false
)
{
	*self = load(allocator, stream, separator, buffer_size, has_header, allow_non_spec_line_endings)!;
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
	(void)self.parse_row()!;
}

macro void? @each_row(InStream stream, String separator = ",", int max_rows = int::max; @body(String[] row)) @maydiscard @deprecated
{
	while (max_rows--)
	{
		@stack_mem(512 ; mem)
		{
			String? s = io::readline(mem, stream);
			if (catch err = s)
			{
				if (err == io::EOF) return;
				return err~;
			}
			@body(s.split(mem, separator));
		};
	}
}

macro void? CsvReader.@each_row(self, int rows = int::max; @body(String[] row)) @maydiscard @deprecated
{
	return @each_row(self.stream, self.separator, rows ; row)
	{
		@body(row);
	};
}

module std::encoding::csv @private;
import std::io, std::collections::list;

fn CsvRow? CsvReader.parse_row(&self)
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

	return {
		parse_row_entries(self.allocator, row.copy_str(self.allocator), self.separator, self.headers.len),
		row.copy_str(self.allocator),
		self.allocator
	};
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
