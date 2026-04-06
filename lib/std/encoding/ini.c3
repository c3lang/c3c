module std::encoding::ini;
import std::io, std::collections::object;

// Constant for the implicit section that holds keys before any section header.
// Pass ini::GLOBAL as the section argument to get/set for those keys.
const String GLOBAL = "";

faultdef INVALID_SECTION, INVALID_KEY, DUPLICATE_KEY;

enum DuplicateKeyBehavior
{
    DUPLICATE_OVERWRITE,
    DUPLICATE_ERROR,
}

struct IniOptions
{
    bool allow_global_keys;              // Keys before the first section (default: true)
    bool allow_hash_comments;            // Treat # as a comment char (default: true)
    bool allow_inline_comments;          // Strip trailing inline comments from values (default: false).
                                         // A comment is recognised as " ;" (or " #" when allow_hash_comments
                                         // is true) — the leading space prevents false positives on values
                                         // like "a=v;data" while still handling "a=v ; comment" correctly.
    char delimiter;                      // Key/value separator: '=' or ':' (default: '=')
    bool pad_delimiter;                  // Surround delimiter with spaces when encoding: "k = v" vs "k=v" (default: true)
    bool trim_values;                    // Strip surrounding whitespace from values (default: true)
    DuplicateKeyBehavior duplicate_keys; // What to do on duplicate keys (default: DUPLICATE_OVERWRITE)
}

const IniOptions DEFAULT_OPTIONS = {
    .allow_global_keys   = true,
    .allow_hash_comments = true,
    .allow_inline_comments = false,
    .delimiter           = '=',
    .pad_delimiter       = true,
    .trim_values         = true,
    .duplicate_keys      = DUPLICATE_OVERWRITE,
};

// ---------------------------------------------------------------------------
// Parsing
// ---------------------------------------------------------------------------

<*
 Parse an INI-formatted string into an Object.

 The returned Object is a map whose keys are section names (or ini::GLOBAL
 for keys that appear before any section header). Each value is itself an
 Object map from key strings to string Objects.

 Call obj.free() when done, or use a temp/arena allocator.
 @param [&inout] allocator
*>
fn Object*? parse(Allocator allocator, String src, IniOptions options = DEFAULT_OPTIONS)
{
    Object* ini = object::new_obj(allocator);
    defer catch ini.free();

    // Only create the GLOBAL section when global keys are actually allowed.
    if (options.allow_global_keys) ini.set(GLOBAL, object::new_obj(allocator));

    String current_section = GLOBAL;

    // Split on "\n"; trim() on each line also strips \r for Windows \r\n files.
    foreach (line : src.split(tmem, "\n"))
    {
        String s = line.trim();
        if (s.len == 0) continue;
        if (s[0] == ';') continue;
        if (options.allow_hash_comments && s[0] == '#') continue;

        if (s[0] == '[')
        {
            // Section header: [name]
            usz? opt_close = s.index_of_char(']');
            if (catch opt_close) return INVALID_SECTION~;
            String name = s[1..opt_close - 1].trim();
            // Reject [] — empty section name would silently alias GLOBAL.
            if (name.len == 0) return INVALID_SECTION~;
            current_section = name;
            if (!ini.has_key(name)) ini.set(name, object::new_obj(allocator));
            continue;
        }

        // Key/value pair
        usz? opt_delim = s.index_of_char(options.delimiter);
        if (catch opt_delim) return INVALID_KEY~;

        if (!options.allow_global_keys && current_section == GLOBAL) return INVALID_KEY~;

        String key = s[:opt_delim].trim();

        // Extract raw value, strip inline comments if enabled, then optionally trim.
        String raw = s[opt_delim + 1..];
        if (options.allow_inline_comments)
        {
            usz? si = raw.index_of(" ;");
            if (try p = si) raw = raw[:p];
            if (options.allow_hash_comments)
            {
                usz? hi = raw.index_of(" #");
                if (try p = hi) raw = raw[:p];
            }
        }
        String value = options.trim_values ? raw.trim() : raw;

        // ini.has_key(current_section) is guaranteed by the section setup above.
        Object* sec = ini.get(current_section)!!;

        if (options.duplicate_keys == DUPLICATE_ERROR && sec.has_key(key)) return DUPLICATE_KEY~;

        sec.set(key, value);
    }

    return ini;
}

fn Object*? tparse(String src, IniOptions options = DEFAULT_OPTIONS)
{
    return parse(tmem, src, options) @inline;
}

// ---------------------------------------------------------------------------
// Serialization
// ---------------------------------------------------------------------------

<*
 Serialize an INI Object back to INI text.

 Expects obj to be a map of section-name -> Object map of key -> string Object,
 as produced by parse().  Section order is not guaranteed.
 @param [&inout] allocator
*>
fn String encode(Allocator allocator, Object* obj, IniOptions options = DEFAULT_OPTIONS)
{
    ByteWriter bw;
    bw.tinit();

    String fmt = options.pad_delimiter ? "%s %c %s\n" : "%s%c%s\n";

    // Global keys first (if any)
    if (try Object* global_sec = obj.get(GLOBAL))
    {
        if (global_sec.is_map())
        {
            global_sec.@each_entry(; String key, Object* val)
            {
                io::fprintf(&bw, fmt, key, options.delimiter, val.s)!!;
            };
            io::fprintf(&bw, "\n")!!;
        }
    }

    // Named sections
    obj.@each_entry(; String section, Object* sec)
    {
        if (section != GLOBAL)
        {
            io::fprintf(&bw, "[%s]\n", section)!!;
            sec.@each_entry(; String key, Object* val)
            {
                io::fprintf(&bw, fmt, key, options.delimiter, val.s)!!;
            };
            io::fprintf(&bw, "\n")!!;
        }
    };

    // bw uses tmem; destroy() would be a no-op on an arena allocator.
    return bw.str_view().copy(allocator);
}

fn String tencode(Object* obj, IniOptions options = DEFAULT_OPTIONS)
{
    return encode(tmem, obj, options) @inline;
}
