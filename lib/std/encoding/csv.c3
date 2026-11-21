<*
 Csv file parser for C3 standard library.

 Features:
 - Support for single or multi-character delimiter.
 - Support for non spec CRLF line endings when setting `allow_non_spec_line_endings` to `true`.
 - Support for csv files without header row by setting `has_header` to `false`.
 - Handling of strings in the data.
 - Handling of double quotes escape in strings `""` --> `"`.
 - Handling of utf8 DOM at the start of the file.
 - Checking for duplicate or emtpy entries in csv header.

 Limitations:
 - Unable to correctly parse non utf8 encoded csv files.
 - Unable to parse utf16 or utf32 files.

 API:
 - `CsvReader`:	Struct containing all information needed for parsing, needs to be initialized.
 - `CsvRow`: 	Struct containing the row data from an csv, should only be obtained from function returns.
 - `load`:		Creates CsvReader ready to be used for parsing.
 - `tload`:		Creates CsvReader using the `tmem` allocator ready to be used for parsing.
 - `@each_row`:	Performes the body on every row in the csv file.

 Methods:
 - `CsvReader.init`:		Initializes the CsvReader to be ready for parsing.
 - `CsvReader.tinit`:		Initializes the CsvReader with the `tmem` allocator to be ready for parsing.
 - `CsvReader.free`:		Frees the allocated memory associated with the CsvReader.
 - `CsvReader.read_row`:	Reads a row of the csv file and returns the pointer to the corresponding CsvRow.
 - `CsvReader.skip_row`:	Reads a row of the csv file and does not return the row.
 - `CsvReader.@each_row`:	Performes the body on every row in the csv file.

 - `CsvRow.to_format`:		Formats the row data for printing.
 - `CsvRow.len`:			Returns the amount of csv entries in the row.
 - `CsvRow.get_col`:		Returns the entry at the provided column number.
 - `CsvRow.free`:			Frees the row entries and the raw row string associated with it.
*>
module std::encoding::csv;
import std::io;

excuse INCONSISTANT_ROW_LENGTH, NON_UNIQUE_ENTRY, INCORRECT_LINE_ENDING, EMPTY_HEADER_ENTRY;

struct CsvReader
{
	InStream stream;
	Allocator allocator;
	CsvRow* headers;
	String delimiter;
	bitstruct flags : char
	{
		bool has_header;
		bool allow_non_spec_line_endings;
		bool has_read;
	}
}

struct CsvRow (Printable)
{
	String[] list;
	String row;
	Allocator allocator;
}

<*
 Create a CsvReader using temporary allocator `tmem`.

 @param stream : "The input stream to read and parse"
 @param delimiter : "Delimiter string, defaults to \",\", also commenly used delimiter is \";\""
 @param has_header : "Boolean flag whether the csv file contains a header with entry names, defaults to true"
 @param allow_non_spec_line_endings : "csv uses \r\n line endings by default, set to true to allow \n and \r"
 @return "The CsvReader using the temp allocator"
 @return? INCONSISTANT_ROW_LENGTH, NON_UNIQUE_ENTRY, INCORRECT_LINE_ENDING, EMPTY_HEADER_ENTRY, encoding::MALFORMED_DATA : "Parsing failure faults."
*>
fn CsvReader? tload(
	InStream stream,
	String delimiter = ",",
	bool has_header = true,
	bool allow_non_spec_line_endings = false,
) => load(tmem, stream, delimiter, has_header, allow_non_spec_line_endings);

<*
 Create a CsvReader.

 @param allocator : "The allocator used when parsing csv rows"
 @param stream : "The input stream to read and parse"
 @param delimiter : "Delimiter string, defaults to \",\", also commenly used delimiter is \";\""
 @param has_header : "Boolean flag whether the csv file contains a header with entry names, defaults to true"
 @param allow_non_spec_line_endings : "csv uses \r\n line endings by default, set to true to allow \n and \r"
 @return "The CsvReader using the temp allocator"
 @return? INCONSISTANT_ROW_LENGTH, NON_UNIQUE_ENTRY, INCORRECT_LINE_ENDING, EMPTY_HEADER_ENTRY, encoding::MALFORMED_DATA : "Parsing failure faults."
*>
fn CsvReader? load(
	Allocator allocator,
	InStream stream,
	String delimiter = ",",
	bool has_header = true,
	bool allow_non_spec_line_endings = false,
)
{
	CsvReader reader;
	reader.allocator = allocator;
	reader.stream = stream;
	reader.delimiter = delimiter;
	reader.flags.has_header = has_header;
	reader.flags.allow_non_spec_line_endings = allow_non_spec_line_endings;
	reader.flags.has_read = false;
	if (has_header)
	{
		reader.headers = reader.read_row()!;
		check_duplicate_entries(reader.headers, false)!;
	}
	return reader;
}

<*
 Initializes a CsvReader using temporaty allocator `tmem`.

 @param stream : "The input stream to read and parse"
 @param delimiter : "Delimiter string, defaults to \",\", also commenly used delimiter is \";\""
 @param has_header : "Boolean flag whether the csv file contains a header with entry names, defaults to true"
 @param allow_non_spec_line_endings : "csv uses \r\n line endings by default, set to true to allow \n and \r"
 @return? INCONSISTANT_ROW_LENGTH, NON_UNIQUE_ENTRY, INCORRECT_LINE_ENDING, EMPTY_HEADER_ENTRY, encoding::MALFORMED_DATA : "Parsing failure faults."
*>
fn void? CsvReader.tinit(
	&self,
	InStream stream,
	String delimiter = ",",
	bool has_header = true,
	bool allow_non_spec_line_endings = false,
) => self.init(
	tmem,
	stream,
	delimiter,
	has_header,
	allow_non_spec_line_endings,
);

<*
 Initializes a CsvReader.

 @param allocator : "The allocator used when parsing csv rows"
 @param stream : "The input stream to read and parse"
 @param delimiter : "Delimiter string, defaults to \",\", also commenly used delimiter is \";\""
 @param has_header : "Boolean flag whether the csv file contains a header with entry names, defaults to true"
 @param allow_non_spec_line_endings : "csv uses \r\n line endings by default, set to true to allow \n and \r"
 @return? INCONSISTANT_ROW_LENGTH, NON_UNIQUE_ENTRY, INCORRECT_LINE_ENDING, EMPTY_HEADER_ENTRY, encoding::MALFORMED_DATA : "Parsing failure faults."
*>
fn void? CsvReader.init(
	&self,
	Allocator allocator,
	InStream stream,
	String delimiter = ",",
	bool has_header = true,
	bool allow_non_spec_line_endings = false,
)
{
	*self = load(allocator, stream, delimiter, has_header, allow_non_spec_line_endings)!;
}

<*
 Frees the CsvReader and the header entries.

 @require self.allocator != null : "CsvReader is already freed or not initialized"
*>
fn void CsvReader.free(&self)
{
	if (self.headers) self.headers.free();
	self.allocator = null;
}

fn sz? CsvRow.to_format(&self, Formatter* f) @dynamic
{
	return f.printf("%s", self.list);
}

fn sz CsvRow.len(&self) @operator(len)
{
	if (self == null) return 0;
	return self.list.len;
}

<*
 @require col < self.list.len : "Column out of range for available entries"
*>
fn String CsvRow.get_col(&self, sz col) @operator([])
{
	return self.list[col];
}

<*
 Frees the CsvRow entries and the underlying string.

 @require self.allocator != null : "Row already freed"
*>
fn void CsvRow.free(&self)
{
	alloc::free(self.allocator, self.list);
	alloc::free(self.allocator, self.row);
	self.allocator = null;
}

<*
 Reads a row of the csv file and discards it.

 @require self.allocator != null : "CsvReader is not initialized"
 @return? encoding::MALFORMED_DATA : "Parsing faults, may also be a stream reading error"
*>
fn void? CsvReader.skip_row(self) @maydiscard => @pool()
{
	(void)find_row(self.allocator, self.stream, self.flags.allow_non_spec_line_endings)!;
}

<*
 Reads a row of the csv file and parses it to a CsvRow.

 @require self.allocator != null : "CsvReader is not initialized"
 @return "A CsvRow containing the row as a string and a list of entries"
 @return? encoding::MALFORMED_DATA : "Parsing faults, may also be a stream reading error"
*>
fn CsvRow*? CsvReader.read_row(&self)
{
	CsvRow* result = alloc::alloc(self.allocator, CsvRow);
	
	result.allocator = self.allocator;
	result.row = find_row(self.allocator, self.stream, self.flags.allow_non_spec_line_endings)!;
	result.list = parse_row_entries(self.allocator, result.row, self.delimiter, self.headers, (char*)&self.flags)!;

	return result;
}

<*
 Parses a csv file and performes the body on each row where the body takes a list of strings as argument.
 This function expects no header row present in the csv file

 @param allocator : "The allocator used when parsing csv rows"
 @param stream : "The input stream to read and parse"
 @param delimiter : "Delimiter string, defaults to \",\", also commenly used delimiter is \";\""
 @param allow_non_spec_line_endings : "Csv uses \r\n line endings by default, set to true to allow \n and \r"
 @param max_rows : "Maximum amount of rows that are allowed to be parsed"
 @return? INCONSISTANT_ROW_LENGTH, NON_UNIQUE_ENTRY, INCORRECT_LINE_ENDING, EMPTY_HEADER_ENTRY, encoding::MALFORMED_DATA : "Parsing failure faults."
*>
macro void? @each_row(
	Allocator allocator,
	InStream stream,
	String delimiter = ",",
	bool allow_non_spec_line_endings = false,
	int max_rows = int::max; @body(String[] row)
) @maydiscard
{
	CsvReader r;
	r.init(allocator, stream, delimiter, false, allow_non_spec_line_endings)!;
	defer r.free();

	return r.@each_row(max_rows ; row)
	{
		@body(row);
	};
}

<*
 Uses the CsvReader to parse each row and performes the body where the body takes a list of strings as argument.

 @param max_rows : "Maximum amount of rows that are allowed to be parsed"
 @return? INCONSISTANT_ROW_LENGTH, NON_UNIQUE_ENTRY, INCORRECT_LINE_ENDING, EMPTY_HEADER_ENTRY, encoding::MALFORMED_DATA : "Parsing failure faults."
*>
macro void? CsvReader.@each_row(&self, int max_rows = int::max; @body(String[] row)) @maydiscard
{
	while (max_rows--)
	{
		@stack_mem(512 ; mem)
		{
			String? s = find_row(self.allocator, self.stream, self.flags.allow_non_spec_line_endings);
			if (catch err = s)
			{
				if (err == io::EOF) return;
				return err~;
			}
			String[] row = parse_row_entries(
				self.allocator,
				s,
				self.delimiter,
				self.headers,
				(char*)&self.flags
			)!;
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

fn String? find_row(Allocator allocator, InStream stream, bool allow_non_spec_line_endings)
{
	DString row;
	row.init(allocator, 256);
	defer row.free();
	ReadState state = NORMAL;
	char? current;
	bool ending     = false;

	while (!ending)
	{
		current = stream.read_byte();
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
		if (!row.len() && allow_non_spec_line_endings && current == '\n') continue;

		switch (state)
		{
			case ENDING:
				if (allow_non_spec_line_endings)
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
					char? next = stream.read_byte();
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
	return row.copy_str(allocator);
}

enum ParseState
{
	STRING,
	NORMAL,
	PUSH,
}

fn String[]? parse_row_entries(Allocator allocator, String row, String delimiter, CsvRow* header = null, char* flags)
{
	if (!row.len) return { };
	sz capacity      = header ? header.len() : 16;
	String* holder   = alloc::alloc_array(allocator, String, capacity);
	sz entry_index   = 0;
	sz row_index     = 0;
	sz reading_index = 0;
	ParseState state = NORMAL;
	char current;

	while (true)
	{
		if (!(*flags & 0b00000100))
		{
			if (row.starts_with(x'EFBBBF')) row_index += 3;
			*flags = *flags ^ 0b00000100;
		}
		if (row_index + reading_index == row.len)
		{
			if (entry_index == capacity)
			{
				capacity *= 2;
				holder = alloc::realloc(allocator, holder, String::size * capacity);
			}
			holder[entry_index++] = row[row_index:reading_index].trim();
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
				if (current == delimiter[0] && row[row_index + reading_index:delimiter.len] == delimiter)
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
				holder[entry_index++] = row[row_index:reading_index].trim();
				row_index += reading_index + delimiter.len;
				reading_index = 0;
		}
	}
	if (header && entry_index != header.len()) return INCONSISTANT_ROW_LENGTH~;

	return holder[:entry_index];
}

fn void? check_duplicate_entries(CsvRow* row, bool allow_empty = true)
{
	if (!allow_empty) foreach (entry : row) if (!entry.len) return EMPTY_HEADER_ENTRY~;
	for (sz i = 0; i < row.len() - 1; i++)
	{
		for (sz j = i + 1; j < row.len(); j++)
		{
			if (row.get_col(i) == row.get_col(j)) return NON_UNIQUE_ENTRY~;
		}
	}
}
