module std::encoding::ini;
import std::io, std::collections::object;

// Constant for the implicit section that holds keys before any section header.
// Pass ini::GLOBAL as the section argument to get/set for those keys.
const String GLOBAL = "";

faultdef INVALID_SECTION, INVALID_KEY, DUPLICATE_KEY;

enum DuplicateKeyBehavior
{
	OVERWRITE,
	ERROR,
}



// ---------------------------------------------------------------------------
// Parsing
// ---------------------------------------------------------------------------

<*
 Parse an INI-formatted stream into an Object.

 The returned Object is a map whose keys are section names (or ini::GLOBAL
 for keys that appear before any section header). Each value is itself an
 Object map from key strings to string Objects.

 Call obj.free() when done, or use a temp/arena allocator.
 @param [&inout] allocator
 @param stream                 : "The stream to read from"
 @param delimiter              : "Key/value separator: '=' or ':' (default: '=')"
 @param trim_values            : "Strip surrounding whitespace from values (default: true)"
 @param allow_global_keys      : "Keys before the first section (default: true)"
 @param allow_hash_comments    : "Treat # as a comment char (default: true)"
 @param allow_inline_comments  : "Strip trailing inline comments from values (default: false)."
 @param on_duplicate_keys      : "What to do on duplicate keys (default: OVERWRITE)"

 @return "The data as an Object"
 @return? INVALID_SECTION      : "If a section is invalid"
 @return? INVALID_KEY          : "If a key is invalid"
 @return? DUPLICATE_KEY        : "If a key appears more than once"
*>
fn Object*? load(
	Allocator allocator, InStream stream,
	char delimiter = '=', bool trim_values = true, bool allow_global_keys = true,
	bool allow_hash_comments = true, bool allow_inline_comments = false,
	DuplicateKeyBehavior on_duplicate_keys = OVERWRITE) => @pool()
{
	Object* ini = object::new_obj(allocator);
	defer catch ini.free();

	// Only create the GLOBAL section when global keys are actually allowed.
	if (allow_global_keys) ini.set(GLOBAL, object::new_obj(allocator));

	String current_section = GLOBAL;

	// Split on "\n"; trim() on each line also strips \r for Windows \r\n files.
	while (try line = io::treadline(stream))
	{
		String s = line.trim();
		if (!s.len) continue;
		switch (s[0])
		{
			case ';':
				continue;
			case '#':
				if (allow_hash_comments) continue;
			case '[':
				// Section header: [name]
				if (s[^1] != ']') return INVALID_SECTION~;
				String name = s[1..^2].trim();
				// Reject [] — empty section name would silently alias GLOBAL.
				if (!name.len) return INVALID_SECTION~;
				current_section = name;
				if (!ini.has_key(name)) ini.set(name, object::new_obj(allocator));
				continue;
			default:
				break;
		}
		// Key/value pair
		sz? delimiter_index = s.index_of_char(delimiter);
		if (catch delimiter_index) return INVALID_KEY~;

		if (!allow_global_keys && current_section == GLOBAL) return INVALID_KEY~;

		String key = s[:delimiter_index].trim();

		// Extract raw value, strip inline comments if enabled, then optionally trim.
		String raw = s[delimiter_index + 1..];
		if (allow_inline_comments)
		{
			if (try p = raw.index_of(" ;"))
			{
				raw = raw[:p];
			}
			else if (allow_hash_comments)
			{
				if (try p = raw.index_of(" #")) raw = raw[:p];
			}
		}
		String value = trim_values ? raw.trim() : raw;

		// ini.has_key(current_section) is guaranteed by the section setup above.
		Object* sec = ini.get(current_section)!!;

		if (on_duplicate_keys == ERROR && sec.has_key(key)) return DUPLICATE_KEY~;

		sec.set(key, value);
	}

	return ini;
}

fn Object*? load_temp(
	InStream stream,
	char delimiter = '=', bool trim_values = true, bool allow_global_keys = true,
	bool allow_hash_comments = true, bool allow_inline_comments = false,
	DuplicateKeyBehavior on_duplicate_keys = OVERWRITE)
{
    return load(tmem, stream, delimiter, trim_values, allow_global_keys, allow_hash_comments, allow_inline_comments, on_duplicate_keys);
}

<*
 Parse an INI-formatted stream into an Object.

 The returned Object is a map whose keys are section names (or ini::GLOBAL
 for keys that appear before any section header). Each value is itself an
 Object map from key strings to string Objects.

 Call obj.free() when done, or use a temp/arena allocator.
 @param [&inout] allocator
 @param string                 : "The string to read from"
 @param delimiter              : "Key/value separator: '=' or ':' (default: '=')"
 @param trim_values            : "Strip surrounding whitespace from values (default: true)"
 @param allow_global_keys      : "Keys before the first section (default: true)"
 @param allow_hash_comments    : "Treat # as a comment char (default: true)"
 @param allow_inline_comments  : "Strip trailing inline comments from values (default: false)."
 @param on_duplicate_keys      : "What to do on duplicate keys (default: OVERWRITE)"

 @return "The data as an Object"
 @return? INVALID_SECTION      : "If a section is invalid"
 @return? INVALID_KEY          : "If a key is invalid"
 @return? DUPLICATE_KEY        : "If a key appears more than once"
*>
fn Object*? parse(
	Allocator allocator, String string,
	char delimiter = '=', bool trim_values = true, bool allow_global_keys = true,
	bool allow_hash_comments = true, bool allow_inline_comments = false,
	DuplicateKeyBehavior on_duplicate_keys = OVERWRITE) => @pool()
{
	ByteReader reader;
	reader.init(string);
	return load(allocator, &reader, delimiter, trim_values, allow_global_keys, allow_hash_comments, allow_inline_comments, on_duplicate_keys);
}

fn Object*? tparse(
	String string,
	char delimiter = '=', bool trim_values = true, bool allow_global_keys = true,
	bool allow_hash_comments = true, bool allow_inline_comments = false,
	DuplicateKeyBehavior on_duplicate_keys = OVERWRITE)
{
    return parse(tmem, string, delimiter, trim_values, allow_global_keys, allow_hash_comments, allow_inline_comments, on_duplicate_keys);
}

// ---------------------------------------------------------------------------
// Serialization
// ---------------------------------------------------------------------------

<*
 Serialize an INI Object back to INI text.

 Expects obj to be a map of section-name -> Object map of key -> string Object,
 as produced by parse().  Section order is not guaranteed.
 @param [&inout] allocator : "The allocator to use for allocating the resulting string"
 @param delimiter              : "Key/value separator: '=' or ':' (default: '=')"
 @param pad_delimiter          : "Surround delimiter with spaces when encoding: 'k = v' vs 'k=v' (default: true)"

 @return "The encoded string"
*>
fn String encode(Allocator allocator, Object* obj, char delimiter = '=', bool pad_delimiter = true) => @pool()
{
	DString d;
	save(&d, obj, delimiter, pad_delimiter)!!;
	return d.copy_str(allocator);
}

<*
 Serialize an INI Object back to INI text.

 Expects obj to be a map of section-name -> Object map of key -> string Object,
 as produced by parse().  Section order is not guaranteed.
 @param stream                 : "The stream to write to"
 @param delimiter              : "Key/value separator: '=' or ':' (default: '=')"
 @param pad_delimiter          : "Surround delimiter with spaces when encoding: 'k = v' vs 'k=v' (default: true)"

 @return "The encoded string"
*>
fn long? save(OutStream stream, Object* obj, char delimiter = '=', bool pad_delimiter = true)
{
	String fmt = pad_delimiter ? "%s %c %s" : "%s%c%s";

	long len = 0;
	// Global keys first (if any)
	if (try Object* global_sec = obj.get(GLOBAL) && global_sec.is_map())
	{
		global_sec.@each_entry(; String key, Object* val)
		{
			len += io::fprintfn(stream, fmt, key, delimiter, val.s)!;
		};
		len += io::fprintn(stream)!;
	}

	// Named sections
	obj.@each_entry(; String section, Object* sec)
	{
		if (section != GLOBAL)
		{
			len += io::fprintfn(stream, "[%s]", section)!;
			sec.@each_entry(; String key, Object* val)
			{
				len += io::fprintfn(stream, fmt, key, delimiter, val.s)!;
			};
			len += io::fprintn(stream)!;
		}
	};
	return len;
}

fn String tencode(Object* obj, char delimiter = '=', bool pad_delimiter = true)
{
	DString d;
	d.tinit(512);
	save(&d, obj, delimiter, pad_delimiter)!!;
	return d.str_view();
}
