module std::encoding::ini;
import std::io, std::collections::map;

// Constant for the implicit section that holds keys before any section header.
// Pass ini::GLOBAL as the section argument to get/set/has_key for those keys.
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

struct IniMap
{
    Allocator allocator;
    HashMap{String, HashMap{String, String}} sections;
}
// No @operator([]): lookup needs two keys (section + key), so a single []
// has no natural meaning and chaining would bypass ownership semantics.

// ---------------------------------------------------------------------------
// Creation (for encoding/writing)
// ---------------------------------------------------------------------------

<*
 Create an empty IniMap for programmatic construction.
 @param [&inout] allocator
*>
fn IniMap* IniMap.init(&self, Allocator allocator)
{
    self.allocator = allocator;
    self.sections.init(allocator);
    _ensure_section(self, GLOBAL);
    return self;
}

fn IniMap* IniMap.tinit(&self)
{
    return self.init(tmem) @inline;
}

// ---------------------------------------------------------------------------
// Parsing (for decoding/reading)
// ---------------------------------------------------------------------------

<*
 Parse an INI-formatted string.
 @param [&inout] allocator
*>
fn IniMap? parse(Allocator allocator, String src, IniOptions options = DEFAULT_OPTIONS)
{
    IniMap m;
    m.allocator = allocator;
    m.sections.init(allocator);
    // Only create the GLOBAL section when global keys are actually allowed.
    if (options.allow_global_keys) _ensure_section(&m, GLOBAL);

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
            if (catch opt_close)
            {
                m.free();
                return INVALID_SECTION~;
            }
            String name = s[1..opt_close - 1].trim();
            // Reject [] — empty section name would silently alias GLOBAL.
            if (name.len == 0)
            {
                m.free();
                return INVALID_SECTION~;
            }
            current_section = name;          // slice into src, valid until loop ends
            _ensure_section(&m, name);       // copies the name into the allocator
            continue;
        }

        // Key/value pair
        usz? opt_delim = s.index_of_char(options.delimiter);
        if (catch opt_delim)
        {
            m.free();
            return INVALID_KEY~;
        }

        if (!options.allow_global_keys && current_section == GLOBAL)
        {
            m.free();
            return INVALID_KEY~;
        }

        String key = s[:opt_delim].trim().copy(allocator);

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
        String value = options.trim_values ? raw.trim().copy(allocator) : raw.copy(allocator);

        // _ensure_section guarantees the entry exists; get_ref!! is safe here.
        HashMap{String, String}* sec = m.sections.get_ref(current_section)!!;

        if (options.duplicate_keys == DUPLICATE_ERROR && sec.has_key(key))
        {
            key.free(allocator);
            value.free(allocator);
            m.free();
            return DUPLICATE_KEY~;
        }

        sec.set(key, value);
    }

    return m;
}

fn IniMap? tparse(String src, IniOptions options = DEFAULT_OPTIONS)
{
    return parse(tmem, src, options) @inline;
}

// ---------------------------------------------------------------------------
// Mutation
// ---------------------------------------------------------------------------

<*
 Insert or update a key/value pair in the given section.
 Copies all strings. Creates the section if it does not exist.
*>
fn void IniMap.set(&self, String section, String key, String value)
{
    _ensure_section(self, section);
    // _ensure_section guarantees the entry exists; get_ref!! is safe here.
    HashMap{String, String}* sec = self.sections.get_ref(section)!!;

    // If key already exists, free the old value and update in-place.
    if (try String* existing = sec.get_ref(key))
    {
        existing.free(self.allocator);
        *existing = value.copy(self.allocator);
    }
    else
    {
        sec.set(key.copy(self.allocator), value.copy(self.allocator));
    }
}

<*
 Remove a single key from a section.
 @return "true if the key existed and was removed"
*>
fn bool IniMap.remove_key(&self, String section, String key)
{
    HashMap{String, String}*? opt_sec = self.sections.get_ref(section);
    if (catch opt_sec) return false;

    var opt_entry @safeinfer = opt_sec.get_entry(key);
    if (catch opt_entry) return false;

    // Save pointers before remove invalidates the entry.
    String stored_key   = opt_entry.key;
    String stored_value = opt_entry.value;

    opt_sec.remove(key)!!;
    stored_key.free(self.allocator);
    stored_value.free(self.allocator);
    return true;
}

<*
 Remove an entire section and all its keys.
 The GLOBAL section ("") cannot be removed.
 @return "true if the section existed and was removed"
*>
fn bool IniMap.remove_section(&self, String section)
{
    if (section.len == 0) return false;

    // Use get_ref to operate directly on the stored inner HashMap (not a copy).
    HashMap{String, String}*? opt_sec = self.sections.get_ref(section);
    if (catch opt_sec) return false;

    // Use get_entry only to retrieve the stored section-name key for freeing.
    var opt_entry @safeinfer = self.sections.get_entry(section);
    String stored_name = opt_entry!!.key;

    // Free all keys and values inside the section, then the inner map itself.
    opt_sec.@each(; String key, String value)
    {
        key.free(self.allocator);
        value.free(self.allocator);
    };
    opt_sec.free();

    self.sections.remove(section)!!;
    stored_name.free(self.allocator);
    return true;
}

// ---------------------------------------------------------------------------
// Lookup
// ---------------------------------------------------------------------------

<*
 @return "the value for (section, key), or NOT_FOUND if absent"
*>
fn String? IniMap.get(&self, String section, String key)
{
    HashMap{String, String}*? opt_sec = self.sections.get_ref(section);
    if (catch opt_sec) return NOT_FOUND~;
    return opt_sec.get(key);
}

<*
 @return "a pointer to the inner HashMap for section, or NOT_FOUND if absent"
*>
fn HashMap{String, String}*? IniMap.get_section(&self, String section)
{
    return self.sections.get_ref(section);
}

<*
 @return "true if section exists"
*>
fn bool IniMap.has_section(&self, String section)
{
    return self.sections.has_key(section);
}

<*
 @return "true if key exists in section"
*>
fn bool IniMap.has_key(&self, String section, String key)
{
    HashMap{String, String}*? opt_sec = self.sections.get_ref(section);
    if (catch opt_sec) return false;
    return opt_sec.has_key(key);
}

<*
 @return "number of sections (the global section is excluded when empty)"
*>
fn usz IniMap.len(&self)
{
    usz count = 0;
    self.sections.@each(; String section, HashMap{String, String} sec)
    {
        if (section != GLOBAL || sec.len() > 0) count++;
    };
    return count;
}

<*
 @return "number of keys in the given section, or 0 if absent"
*>
fn usz IniMap.section_len(&self, String section)
{
    HashMap{String, String}*? opt_sec = self.sections.get_ref(section);
    if (catch opt_sec) return 0;
    return opt_sec.len();
}

// ---------------------------------------------------------------------------
// Iteration
// ---------------------------------------------------------------------------

<*
 Returns a temp-allocated slice of section names.
 The empty GLOBAL section ("") is excluded, matching the behaviour of len().
 When GLOBAL has keys it IS included as ""; use ini::GLOBAL to test for it:

   foreach (section : m.iter())
   {
       if (section == ini::GLOBAL) { /* handle global keys */ continue; }
       // ...
   }
*>
fn String[] IniMap.iter(&self)
{
    usz n = self.len();
    String[] names = alloc::alloc_array(tmem, String, n);
    usz i = 0;
    self.sections.@each(; String section, HashMap{String, String} sec)
    {
        if (section != GLOBAL || sec.len() > 0) names[i++] = section;
    };
    return names;
}

<*
 Iterate over all key/value pairs in a given section.
*>
macro void IniMap.@each_key(&self, String section; @body(String key, String value))
{
    HashMap{String, String}*? opt_sec = self.sections.get_ref(section);
    if (catch opt_sec) return;
    opt_sec.@each(; String key, String value)
    {
        @body(key, value);
    };
}

<*
 Iterate over every entry in the map as (section, key, value).
 `section` may be `ini::GLOBAL` ("") when there are global keys.
 Serialisers should emit global keys before named sections:

   m.@each(; String section, String key, String value)
   {
       if (section == ini::GLOBAL) { /* emit key=value with no header */ }
       else { /* emit [section] header then key=value */ }
   }
*>
macro void IniMap.@each(&self; @body(String section, String key, String value))
{
    self.sections.@each(; String section, HashMap{String, String} sec)
    {
        sec.@each(; String key, String value)
        {
            @body(section, key, value);
        };
    };
}

// ---------------------------------------------------------------------------
// Serialization
// ---------------------------------------------------------------------------

<*
 Serialize the map back to INI text. Section order is not guaranteed.
 @param [&inout] allocator
*>
fn String IniMap.encode(&self, Allocator allocator, IniOptions options = DEFAULT_OPTIONS)
{
    ByteWriter bw;
    bw.tinit();

    String fmt = options.pad_delimiter ? "%s %c %s\n" : "%s%c%s\n";

    // Global keys first (if any)
    if (try HashMap{String, String}* global_sec = self.sections.get_ref(GLOBAL))
    {
        if (global_sec.len() > 0)
        {
            global_sec.@each(; String key, String value)
            {
                io::fprintf(&bw, fmt, key, options.delimiter, value)!!;
            };
            io::fprintf(&bw, "\n")!!;
        }
    }

    // Named sections
    self.sections.@each(; String section, HashMap{String, String} sec)
    {
        if (section != GLOBAL)
        {
            io::fprintf(&bw, "[%s]\n", section)!!;
            sec.@each(; String key, String value)
            {
                io::fprintf(&bw, fmt, key, options.delimiter, value)!!;
            };
            io::fprintf(&bw, "\n")!!;
        }
    };

    // bw uses tmem; destroy() would be a no-op on an arena allocator.
    return bw.str_view().copy(allocator);
}

fn String IniMap.tencode(&self, IniOptions options = DEFAULT_OPTIONS)
{
    return self.encode(tmem, options) @inline;
}

// ---------------------------------------------------------------------------
// Cleanup
// ---------------------------------------------------------------------------

<*
 Remove all entries and sections, keeping the empty GLOBAL section.
 Does not free the IniMap itself.
*>
fn void IniMap.clear(&self)
{
    // @each yields by value, but copies share the same backing pointers, so
    // sec.free() frees the actual table memory. The outer map's clear()/free()
    // only resets slot metadata and never dereferences stored values.
    self.sections.@each(; String section, HashMap{String, String} sec)
    {
        sec.@each(; String key, String value)
        {
            key.free(self.allocator);
            value.free(self.allocator);
        };
        sec.free();
        if (section.len > 0) section.free(self.allocator);
    };
    self.sections.clear();
    _ensure_section(self, GLOBAL);
}

<*
 Free all memory owned by this map, including all copied section names,
 keys, and values. Safe to call on a partially-built map (e.g. after a
 parse error). Do not call when using a temp/arena allocator.
*>
fn void IniMap.free(&self)
{
    // @each yields by value, but copies share the same backing pointers, so
    // sec.free() frees the actual table memory. The outer sections.free()
    // only frees the outer table array and never dereferences stored values.
    self.sections.@each(; String section, HashMap{String, String} sec)
    {
        sec.@each(; String key, String value)
        {
            key.free(self.allocator);
            value.free(self.allocator);
        };
        sec.free();
        // GLOBAL ("") is a constant with len == 0 — was never heap-allocated.
        if (section.len > 0) section.free(self.allocator);
    };
    self.sections.free();
}

// ---------------------------------------------------------------------------
// Private helpers
// ---------------------------------------------------------------------------

// Ensures a section exists, copying the name into the allocator (except GLOBAL).
fn void _ensure_section(IniMap* m, String name) @private
{
    if (m.sections.has_key(name)) return;
    HashMap{String, String} sec;
    sec.init(m.allocator);
    String stored = name.len > 0 ? name.copy(m.allocator) : name;
    m.sections.set(stored, sec);
}
