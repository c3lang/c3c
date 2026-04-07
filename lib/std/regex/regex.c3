<*
 A compact stdlib regex engine with a hybrid execution model.

 Positioning / compatibility:
 - This module aims for a practical, PCRE-like feature set inside the stdlib, but it is not
   a drop-in compatibility target for PCRE2, Oniguruma, .NET, Java, Python, or ECMAScript.
 - The documented semantics in this header are the compatibility contract for C3 regexes.
   When another engine behaves differently, prefer the behavior described here and covered by
   the stdlib tests.

 Matching semantics:
 - `is_match`, `find`, `replace`, and `replace_all` search unanchored by default.
   Anchors such as `^`, `$`, `\A`, `\z`, and `\G` are required for anchored behavior.
 - Search is leftmost-first by start position; when multiple paths match at the same start,
   the engine keeps the longest match that survives the NFA/backtracking execution.
 - UTF-8 literals, escapes, and captures operate on byte offsets but decode codepoints
   when matching.
 - `FLAG_IGNORECASE` performs ASCII folding plus the explicit Latin-1, Greek, and
   Cyrillic pairs handled by `unicode_case_fold`; it is not full Unicode case folding.
 - `\d` and POSIX bracket classes are ASCII-defined.
 - `\s`, `\h`, and `\v` use exact Unicode whitespace tables.
 - `\w`, `\W`, `\b`, and `\B` treat ASCII word characters plus Latin-1 letters as word
   characters.
 - ASCII range classes such as `[A-Z]` remain explicit codepoint ranges, even under
   `FLAG_IGNORECASE`.
 - `\p{...}` / `\P{...}` currently support `ASCII`, `White_Space`, `Horiz_Space`,
   `Vert_Space`, and `Word`.
 - Bare inline `(?x)` and `(?U)` toggles affect parsing and greediness for the remainder
   of the pattern. Bare inline `(?i)`, `(?m)`, `(?s)`, and their `(?-...)` forms are
   accepted for surface compatibility but do not currently alter runtime matching; use the
   corresponding compile flags for ignore-case, multiline, and dotall behavior. Scoped
   inline flag groups are not supported.
 - Variable-width lookbehind scans all prior UTF-8 character boundaries before the current
   position when searching for a valid start.
 - Replacement syntax supports `$$`, `$&`, `$0`..`$9`, `${N}`, and `${name}`. Unknown or
   unmatched capture references expand to the empty string.

 Supported syntax overview:
 - Literals, `.`, alternation `|`, grouping `(...)`, non-capturing `(?:...)`, comments
   `(?#...)`, named groups `(?<name>...)`, and branch reset `(?|...)`.
 - Quantifiers `*`, `+`, `?`, `{n}`, `{n,}`, `{n,m}` with lazy suffixes and possessive
   suffixes where supported.
 - Anchors `^`, `$`, `\A`, `\z`, `\Z`, `\b`, `\B`, `\G`, and linebreak escape `\R`.
 - Lookahead/lookbehind, atomic groups `(?>...)`, backreferences, conditionals, and `\K`.
 - Replacement references `$$`, `$&`, `$0`..`$9`, `${N}`, and `${name}`.

 Ownership / lifetime:
 - `compile` returns a heap-owned `Regex*`; call `Regex.free()` when done.
 - `find` / `find_at` / `MatchIter.next` return heap-owned `Match` values whose capture
   buffers must be released with `Match.free()`.
 - `replace` / `replace_all` return newly allocated strings owned by the caller.

 Limitations / compatibility notes:
 - This engine is intentionally PCRE-like rather than bug-for-bug PCRE-compatible.
 - Unicode support is partial: property support is currently limited to `ASCII`,
   `White_Space`, `Horiz_Space`, `Vert_Space`, and `Word`, and ignore-case matching
   uses explicit folding tables rather than full Unicode case folding.
 - Bare inline `(?i)`, `(?m)`, and `(?s)` are accepted for compatibility but do not
   currently toggle runtime matching; use compile flags instead.

 Examples:
 - Compile and search:
     Regex* re = regex::compile(mem, "\\d+")!!;
     defer re.free();
     Match m = re.find(mem, "abc123")!!;
     defer m.free();
     assert(m.full() == "123");
 - Named captures:
     Regex* re = regex::compile(mem, "(?<key>\\w+)=(?<value>\\d+)")!!;
     defer re.free();
     Match m = re.find(mem, "port=8080")!!;
     defer m.free();
     assert(m.named_group("key")!! == "port");
     assert(m.named_group("value")!! == "8080");
 - Replace all:
     Regex* re = regex::compile(mem, "\\d+")!!;
     defer re.free();
     String out = re.replace_all(mem, "v1 v22", "<$0>")!!;
     defer alloc::free(mem, out.ptr);
     assert(out == "v<1> v<22>");
 - Fixed-width lookbehind:
     Regex* re = regex::compile(mem, "(?<=\\$)\\d+")!!;
     defer re.free();
     Match m = re.find(mem, "price $42")!!;
     defer m.free();
     assert(m.full() == "42");

 Unsupported / intentionally omitted syntax:
 - No scoped inline-flag groups such as `(?i:foo)`.
 - No general Unicode property categories such as `\p{L}` / `\P{Nd}` yet.
*>
module std::regex;
import std::io;
import std::core::string::iterator;
import std::core::ascii;
import std::collections::list;

// ============================================================
// Public faults
// ============================================================

<*
 Public regex faults:
 - `INVALID_PATTERN`: pattern syntax was rejected while parsing.
 - `COMPILE_ERROR`: the pattern parsed, but could not be lowered to an executable regex.
 - `NO_MATCH`: no match was found for the requested operation.
 - `GROUP_NOT_FOUND`: a requested numbered or named capture does not exist or did not match.
*>
faultdef INVALID_PATTERN, COMPILE_ERROR, NO_MATCH, GROUP_NOT_FOUND;

// ============================================================
// Public flags
// ============================================================

typedef RegexFlags = uint;
const RegexFlags FLAG_NONE        = (RegexFlags)0;
const RegexFlags FLAG_IGNORECASE  = (RegexFlags)1;   // i — case-insensitive matching with explicit ASCII/Latin-1/Greek/Cyrillic folding
const RegexFlags FLAG_MULTILINE   = (RegexFlags)2;   // m — ^ and $ match line boundaries
const RegexFlags FLAG_DOTALL      = (RegexFlags)4;   // s — . matches \n too
const RegexFlags FLAG_UNGREEDY    = (RegexFlags)8;   // U — all quantifiers lazy by default
const RegexFlags FLAG_EXTENDED    = (RegexFlags)16;  // x — free-spacing: ignore whitespace and # comments in pattern

// ============================================================
// CharClass — hybrid ASCII bitmask + Unicode codepoint ranges
// ============================================================

struct RegexUnicodeRange
{
	Char32 lo;
	Char32 hi;
}

typedef RegexUnicodePropMask = uint;
const RegexUnicodePropMask PROP_NONE        = (RegexUnicodePropMask)0;
const RegexUnicodePropMask PROP_DIGIT       = (RegexUnicodePropMask)1;
const RegexUnicodePropMask PROP_WORD        = (RegexUnicodePropMask)2;
const RegexUnicodePropMask PROP_WHITE_SPACE = (RegexUnicodePropMask)4;
const RegexUnicodePropMask PROP_HSPACE      = (RegexUnicodePropMask)8;
const RegexUnicodePropMask PROP_VSPACE      = (RegexUnicodePropMask)16;
const RegexUnicodePropMask PROP_ASCII       = (RegexUnicodePropMask)32;

struct RegexCharClass
{
	AsciiCharset ascii_bits;   // bitmask for codepoints 0–127
	RegexUnicodeRange* ranges;      // sorted [lo, hi] pairs for codepoints >= 128; not owned
	usz ranges_count;
	RegexUnicodePropMask props;     // union of special predicate-backed classes
	RegexUnicodePropMask complement_props; // union of predicates whose complements were added inside [...]
	bool negated;
}

alias RegexTempRangeList = List{RegexUnicodeRange};

// Fold a Unicode codepoint to lowercase for case-insensitive comparison.
// Covers ASCII, Latin-1 supplement, basic Greek, and basic Cyrillic one-to-one folds.
fn Char32 unicode_case_fold(Char32 c) @inline
{
	if (c < 0x80) return (c >= 'A' && c <= 'Z') ? c + 0x20 : c;
	if ((c >= 0xC0 && c <= 0xD6) || (c >= 0xD8 && c <= 0xDE)) return c + 0x20;
	if (c == 0x0178) return 0x00FF; // Ÿ -> ÿ
	if (c >= 0x0391 && c <= 0x03A1) return c + 0x20;
	if (c >= 0x03A3 && c <= 0x03AB) return c + 0x20;
	if (c == 0x03C2) return 0x03C3; // final sigma -> sigma
	if (c == 0x0401) return 0x0451; // Ё -> ё
	if (c >= 0x0410 && c <= 0x042F) return c + 0x20;
	return c;
}

// Fold a Unicode codepoint to uppercase (inverse of unicode_case_fold for supported ranges).
fn Char32 unicode_case_unfold(Char32 c) @inline
{
	if (c < 0x80) return (c >= 'a' && c <= 'z') ? c - 0x20 : c;
	if ((c >= 0xE0 && c <= 0xF6) || (c >= 0xF8 && c <= 0xFE)) return c - 0x20;
	if (c == 0x00FF) return 0x0178; // ÿ -> Ÿ
	if (c >= 0x03B1 && c <= 0x03C1) return c - 0x20;
	if (c == 0x03C2) return 0x03A3; // final sigma -> Σ
	if (c >= 0x03C3 && c <= 0x03CB) return c - 0x20;
	if (c == 0x0451) return 0x0401; // ё -> Ё
	if (c >= 0x0430 && c <= 0x044F) return c - 0x20;
	return c;
}

fn bool unicode_is_white_space(Char32 c) @local @inline
{
	switch (c)
	{
		case '\t':
		case '\n':
		case '\v':
		case '\f':
		case '\r':
		case ' ':
		case 0x85:
		case 0xA0:
		case 0x1680:
		case 0x2028:
		case 0x2029:
		case 0x202F:
		case 0x205F:
		case 0x3000:
			return true;
		default:
			return c >= 0x2000 && c <= 0x200A;
	}
}

fn bool unicode_is_hspace(Char32 c) @local @inline
{
	switch (c)
	{
		case '\t':
		case ' ':
		case 0xA0:
		case 0x1680:
		case 0x202F:
		case 0x205F:
		case 0x3000:
			return true;
		default:
			return c >= 0x2000 && c <= 0x200A;
	}
}

fn bool unicode_is_vspace(Char32 c) @local @inline
{
	switch (c)
	{
		case '\n':
		case '\v':
		case '\f':
		case '\r':
		case 0x85:
		case 0x2028:
		case 0x2029:
			return true;
		default:
			return false;
	}
}

fn bool unicode_prop_matches(Char32 c, RegexUnicodePropMask prop) @local @inline
{
	switch (prop)
	{
		case PROP_DIGIT:
			return c < 128 && ascii::NUMBER_SET.contains((char)c);
		case PROP_WORD:
			return is_word_char(c);
		case PROP_WHITE_SPACE:
			return unicode_is_white_space(c);
		case PROP_HSPACE:
			return unicode_is_hspace(c);
		case PROP_VSPACE:
			return unicode_is_vspace(c);
		case PROP_ASCII:
			return c < 128;
		default:
			return false;
	}
}

fn bool unicode_props_match_any(Char32 c, RegexUnicodePropMask mask) @local @inline
{
	if (mask == PROP_NONE) return false;
	if ((mask & PROP_DIGIT) != PROP_NONE && unicode_prop_matches(c, PROP_DIGIT)) return true;
	if ((mask & PROP_WORD) != PROP_NONE && unicode_prop_matches(c, PROP_WORD)) return true;
	if ((mask & PROP_WHITE_SPACE) != PROP_NONE && unicode_prop_matches(c, PROP_WHITE_SPACE)) return true;
	if ((mask & PROP_HSPACE) != PROP_NONE && unicode_prop_matches(c, PROP_HSPACE)) return true;
	if ((mask & PROP_VSPACE) != PROP_NONE && unicode_prop_matches(c, PROP_VSPACE)) return true;
	if ((mask & PROP_ASCII) != PROP_NONE && unicode_prop_matches(c, PROP_ASCII)) return true;
	return false;
}

fn bool unicode_props_match_all(Char32 c, RegexUnicodePropMask mask) @local @inline
{
	if (mask == PROP_NONE) return false;
	if ((mask & PROP_DIGIT) != PROP_NONE && !unicode_prop_matches(c, PROP_DIGIT)) return false;
	if ((mask & PROP_WORD) != PROP_NONE && !unicode_prop_matches(c, PROP_WORD)) return false;
	if ((mask & PROP_WHITE_SPACE) != PROP_NONE && !unicode_prop_matches(c, PROP_WHITE_SPACE)) return false;
	if ((mask & PROP_HSPACE) != PROP_NONE && !unicode_prop_matches(c, PROP_HSPACE)) return false;
	if ((mask & PROP_VSPACE) != PROP_NONE && !unicode_prop_matches(c, PROP_VSPACE)) return false;
	if ((mask & PROP_ASCII) != PROP_NONE && !unicode_prop_matches(c, PROP_ASCII)) return false;
	return true;
}

fn bool RegexCharClass.matches(self, Char32 c, bool icase)
{
	bool hit;
	if (c < 128)
	{
		char ac = (char)c;
		hit = self.ascii_bits.contains(ac);
		if (icase && !hit)
		{
			char lower = ascii::@to_lower(ac);
			char upper = ascii::@to_upper(ac);
			hit = self.ascii_bits.contains(lower) || self.ascii_bits.contains(upper);
		}
	}
	else
	{
		// Try the character as-is, then its case-folded and case-unfolded forms
		Char32[3] forms = { c, unicode_case_fold(c), unicode_case_unfold(c) };
		int n_forms = icase ? 3 : 1;
		hit = false;
		for (int fi = 0; fi < n_forms && !hit; fi++)
		{
			Char32 cv = forms[fi];
			usz lo = 0;
			usz hi = self.ranges_count;
			while (lo < hi)
			{
				usz mid = lo + (hi - lo) / 2;
				RegexUnicodeRange r = self.ranges[mid];
				if (cv < r.lo)      { hi = mid; }
				else if (cv > r.hi) { lo = mid + 1; }
				else                { hit = true; break; }
			}
		}
	}
	if (!hit && unicode_props_match_any(c, (RegexUnicodePropMask)self.props))
	{
		hit = true;
	}
	if (self.complement_props != PROP_NONE && !unicode_props_match_all(c, (RegexUnicodePropMask)self.complement_props))
	{
		hit = true;
	}
	return self.negated ? !hit : hit;
}

fn usz utf8_codepoint_width(Char32 c) @inline
{
	if (c <= 0x7F) return 1;
	if (c <= 0x7FF) return 2;
	if (c <= 0xFFFF) return 3;
	return 4;
}

fn bool is_utf8_continuation_byte(char c) @local @inline => (c & 0xC0) == 0x80;

fn usz next_utf8_codepoint_start(String input, usz pos, usz limit) @inline
{
	pos++;
	while (pos < limit && is_utf8_continuation_byte(input[pos])) pos++;
	return pos;
}

fn usz prev_utf8_codepoint_start(String input, usz pos) @local @inline
{
	if (pos == 0) return 0;
	pos--;
	while (pos > 0 && is_utf8_continuation_byte(input[pos])) pos--;
	return pos;
}

fn Char32 codepoint_at(String input, usz pos)
{
	if (pos >= input.len) return 0;
	StringIterator it = { .utf8 = input, .current = pos };
	Char32? cp = it.next();
	if (catch cp) return 0;
	return cp;
}

fn Char32 previous_codepoint_before(String input, usz pos)
{
	if (pos == 0) return 0;
	return codepoint_at(input, prev_utf8_codepoint_start(input, pos));
}

fn bool is_word_position(String input, usz pos)
{
	return pos < input.len && is_word_char(codepoint_at(input, pos));
}

fn usz? charclass_fixed_width(RegexCharClass cls)
{
	if (cls.negated) return INVALID_PATTERN~;
	if (cls.complement_props != PROP_NONE) return INVALID_PATTERN~;

	usz width = 0;
	bool has_any = false;
	if (cls.ascii_bits != (AsciiCharset)0)
	{
		width = 1;
		has_any = true;
	}
	for (usz i = 0; i < cls.ranges_count; i++)
	{
		RegexUnicodeRange r = cls.ranges[i];
		usz lo_w = utf8_codepoint_width(r.lo);
		usz hi_w = utf8_codepoint_width(r.hi);
		if (lo_w != hi_w) return INVALID_PATTERN~;
		if (has_any && width != lo_w) return INVALID_PATTERN~;
		width = lo_w;
		has_any = true;
	}
	RegexUnicodePropMask prop_mask = (RegexUnicodePropMask)cls.props;
	if ((prop_mask & (PROP_WHITE_SPACE | PROP_HSPACE | PROP_VSPACE)) != PROP_NONE) return INVALID_PATTERN~;
	if ((prop_mask & (PROP_DIGIT | PROP_WORD | PROP_ASCII)) != PROP_NONE)
	{
		if (has_any && width != 1) return INVALID_PATTERN~;
		width = 1;
		has_any = true;
	}
	return has_any ? width : INVALID_PATTERN~;
}

// Build a charset for a contiguous ASCII range [lo, hi]
macro AsciiCharset @ascii_range_set(int $lo, int $hi) @const
{
	var $s = (AsciiCharset)0;
	$for var $c = $lo; $c <= $hi; $c++:
		$s |= (AsciiCharset)1 << $c;
	$endfor
	return $s;
}

// POSIX-class charsets
const AsciiCharset XDIGIT_SET   = ascii::@create_set("0123456789abcdefABCDEF");
const AsciiCharset BLANK_SET    = ascii::@create_set(" \t");
const AsciiCharset WORD_SET     = ascii::ALPHANUMERIC_SET | ((AsciiCharset)1 << '_');
// ASCII subset of horizontal whitespace; Unicode handling is provided by PROP_HSPACE.
const AsciiCharset HSPACE_SET   = ascii::@create_set(" \t");
// ASCII subset of vertical whitespace; Unicode handling is provided by PROP_VSPACE.
const AsciiCharset VSPACE_SET   = ascii::@create_set("\n\v\f\r");
// Print: ASCII 32..126
const AsciiCharset PRINT_SET    = @ascii_range_set(32, 126);
// Graph: ASCII 33..126 (printable non-space)
const AsciiCharset GRAPH_SET    = @ascii_range_set(33, 126);
// Cntrl: ASCII 0..31 + 127
const AsciiCharset CNTRL_LOW    = @ascii_range_set(0, 31);
const AsciiCharset CNTRL_SET    = CNTRL_LOW | ((AsciiCharset)1 << 127);
// Punct: graph but not alnum
const AsciiCharset PUNCT_SET    = ascii::@create_set("!\"#$%&'()*+,-./:;<=>?@[\\]^_`{|}~");

// Predefined CharClass constants
const RegexCharClass CLASS_DIGIT      = { .ascii_bits = ascii::NUMBER_SET,      .props = PROP_DIGIT };
const RegexCharClass CLASS_DIGIT_INV  = { .ascii_bits = ascii::NUMBER_SET,      .props = PROP_DIGIT,       .negated = true };
const RegexCharClass CLASS_WORD       = { .ascii_bits = WORD_SET,               .props = PROP_WORD };
const RegexCharClass CLASS_WORD_INV   = { .ascii_bits = WORD_SET,               .props = PROP_WORD,        .negated = true };
const RegexCharClass CLASS_SPACE      = { .ascii_bits = ascii::WHITESPACE_SET,  .props = PROP_WHITE_SPACE };
const RegexCharClass CLASS_SPACE_INV  = { .ascii_bits = ascii::WHITESPACE_SET,  .props = PROP_WHITE_SPACE, .negated = true };
// DOT: matches everything except '\n'; compared by ascii_bits value in simulation
const RegexCharClass CLASS_DOT        = { .ascii_bits = ~((AsciiCharset)1 << '\n') };
// Horizontal/vertical whitespace classes
const RegexCharClass CLASS_HSPACE     = { .ascii_bits = HSPACE_SET, .props = PROP_HSPACE };
const RegexCharClass CLASS_HSPACE_INV = { .ascii_bits = HSPACE_SET, .props = PROP_HSPACE, .negated = true };
const RegexCharClass CLASS_VSPACE     = { .ascii_bits = VSPACE_SET, .props = PROP_VSPACE };
const RegexCharClass CLASS_VSPACE_INV = { .ascii_bits = VSPACE_SET, .props = PROP_VSPACE, .negated = true };

// ============================================================
// AST nodes — temporary; allocated from tmem during compile
// ============================================================

enum RegexAstKind : char
{
	AST_LITERAL,
	AST_CHARCLASS,
	AST_CONCAT,
	AST_ALT,
	AST_STAR,
	AST_PLUS,
	AST_QUEST,
	AST_REPEAT,
	AST_GROUP,
	AST_NCGROUP,
	AST_LOOKAHEAD_POS,
	AST_LOOKAHEAD_NEG,
	AST_LOOKBEHIND_POS,
	AST_LOOKBEHIND_NEG,
	AST_ANCHOR_START,
	AST_ANCHOR_END,
	AST_ANCHOR_ABSOLUTE_START,  // \A — always matches start of input
	AST_ANCHOR_ABSOLUTE_END,    // \z — always matches end of input
	AST_ANCHOR_ABSOLUTE_END_NL, // \Z — matches end of input or before final \n
	AST_WORD_BOUND,
	AST_WORD_NBOUND,
	AST_LINEBREAK,              // \R — generic line break (\r\n or single Unicode newline)
	AST_KEEP,                   // \K — reset match start to current position
	AST_NON_NEWLINE,            // \N — non-newline (like . but ignores FLAG_DOTALL)
	AST_BACKREF,                // \1-\9 or \k<name> — backreference to capture group
	AST_ATOMIC,                 // (?>...) — atomic group (no backtracking into sub-expr)
	AST_ANCHOR_GPOS,            // \G — match at start_pos of current find_at call
	AST_COND_GROUP,             // (?(N)yes|no) / (?(name)yes|no) — conditional pattern
	AST_BRANCH_RESET,           // (?|...) — branch reset group (groups reuse IDs across alts)
}

struct RegexAstNode
{
	RegexAstKind kind;
	RegexAstNode* left;
	RegexAstNode* right;
	Char32 codepoint;
	RegexCharClass char_class;
	int group_id;
	String group_name;   // slice into pattern; non-owning
	int repeat_min;
	int repeat_max;      // -1 = unlimited
	bool lazy;
}

// ============================================================
// NFA states
// ============================================================

enum RegexStateKind : char
{
	STATE_LITERAL,
	STATE_CHARSET,
	STATE_SPLIT,
	STATE_MATCH,
	STATE_CAPTURE_START,
	STATE_CAPTURE_END,
	STATE_LOOKAHEAD_POS,
	STATE_LOOKAHEAD_NEG,
	STATE_LOOKBEHIND_POS,
	STATE_LOOKBEHIND_NEG,
	STATE_ANCHOR_START,
	STATE_ANCHOR_END,
	STATE_ANCHOR_ABSOLUTE_START,  // \A
	STATE_ANCHOR_ABSOLUTE_END,    // \z
	STATE_ANCHOR_ABSOLUTE_END_NL, // \Z
	STATE_WORD_BOUND,
	STATE_WORD_NBOUND,
	STATE_KEEP,            // \K — resets match start to current position
	STATE_BACKREF,         // \1-\9 or \k<name> — compare captured text at current pos
	STATE_ATOMIC,          // (?>...) — run sub-NFA, commit to first match (no retry)
	STATE_ANCHOR_GPOS,     // \G — match at re.gpos (start_pos of current find_at call)
	STATE_COND_GROUP,      // (?(N)yes|no) — conditional; backref_id=group, lookaround_start=yes, out2=no
}

struct RegexNfaState
{
	RegexStateKind kind;
	RegexNfaState* out1;
	RegexNfaState* out2;             // SPLIT only
	usz index;                  // position in flat states[] array
	Char32 codepoint;           // STATE_LITERAL
	RegexCharClass char_class;       // STATE_CHARSET
	int group_id;               // CAPTURE_START/END
	RegexNfaState* lookaround_start;   // lookahead/lookbehind sub-NFA entry
	usz lookbehind_len;           // fixed-width lookbehind byte count
	bool lookbehind_variable;     // true = variable-width; scan all start positions
	int backref_id;               // STATE_BACKREF: capture group index (1-based)
	bool lazy;                    // SPLIT only: lazy quantifier
}

// Thompson construction fragment: a start state + dangling output pointers
alias RegexPatchList = List{RegexNfaState**};

struct RegexNfaFrag
{
	RegexNfaState* start;
	RegexPatchList out;
}

// ============================================================
// Named group entry
// ============================================================

struct RegexNamedGroupEntry
{
	String name;      // owned by Regex.allocator
	int group_id;
}

// ============================================================
// Public types
// ============================================================

struct Regex
{
	Allocator       allocator;
	RegexNfaState*  states;
	usz             state_count;
	usz             start_idx;     // index of NFA start state in states[]
	int             group_count;
	RegexNamedGroupEntry* named_groups;
	usz             named_group_count;
	RegexUnicodeRange* range_pool;
	usz             range_pool_count;
	RegexFlags      flags;
	bool            has_backref;   // pattern contains \1-\9; routes to BT engine
	bool            has_atomic;    // pattern contains (?>...); routes to BT engine
	bool            has_gpos;      // pattern contains \G; routes to BT engine
	bool            has_cond;      // pattern contains (?(N)...); routes to BT engine
	bool            has_start_filter; // first consumed codepoint must match start_filter
	RegexCharClass  start_filter;
	usz             gpos;          // set to start_pos before each BT match attempt (for \G)
}

struct MatchGroup
{
	usz  start;
	usz  end;
	bool matched;
}

struct Match
{
	Regex*      regex;
	String      input;
	MatchGroup* groups;     // groups[0] = full match; groups[1..] = captures; owned
	usz         group_count;
	Allocator   allocator;
}

struct MatchIter
{
	Regex*    regex;
	String    input;
	usz       pos;
	Allocator allocator;
}

// ============================================================
// Match methods
// ============================================================

<*
 Returns the full matched substring.

 @return "The full matched substring."
*>
fn String Match.full(self)
{
	MatchGroup g = self.groups[0];
	return self.input[g.start : g.end - g.start];
}

<*
 Returns the byte offset of the start of the full match.

 @return "The byte offset of the start of the full match."
*>
fn usz Match.start(self) => self.groups[0].start;

<*
 Returns the byte offset one past the end of the full match.

 @return "The byte offset one past the end of the full match."
*>
fn usz Match.end(self)   => self.groups[0].end;

<*
 Returns capture group `index`.

 Group `0` is the full match. Returns `GROUP_NOT_FOUND` when the group index is out of
 range or when that capture did not participate in this match.

 @param index : "The capture index. Group 0 is the full match."
 @return "The substring captured by the requested group."
 @return? GROUP_NOT_FOUND : "If the group index is out of range or did not participate in this match."
*>
fn String? Match.group(self, usz index)
{
	if (index >= self.group_count) return GROUP_NOT_FOUND~;
	MatchGroup g = self.groups[index];
	if (!g.matched) return GROUP_NOT_FOUND~;
	return self.input[g.start : g.end - g.start];
}

<*
 Returns the named capture `name`.

 Returns `GROUP_NOT_FOUND` when the name is unknown or when the capture did not
 participate in this match.

 @param name : "The capture name to look up."
 @return "The substring captured by the requested named group."
 @return? GROUP_NOT_FOUND : "If the capture name is unknown or did not participate in this match."
*>
fn String? Match.named_group(self, String name)
{
	Regex* re = self.regex;
	for (usz i = 0; i < re.named_group_count; i++)
	{
		if (re.named_groups[i].name == name)
		{
			return self.group((usz)re.named_groups[i].group_id);
		}
	}
	return GROUP_NOT_FOUND~;
}

<*
 Releases the capture buffer owned by this match.
*>
fn void Match.free(&self)
{
	if (self.groups)
	{
		alloc::free(self.allocator, self.groups);
		self.groups = null;
	}
}

// ============================================================
// MatchIter
// ============================================================

<*
 Returns the next non-overlapping match from the iterator.

 Zero-length matches advance by one UTF-8 codepoint to guarantee forward progress.
 Returns `NO_MATCH` when the iterator is exhausted.

 @return "The next non-overlapping match."
 @return? NO_MATCH : "If the iterator is exhausted."
*>
fn Match? MatchIter.next(&self)
{
	if (self.pos > self.input.len) return NO_MATCH~;
	Match? m = self.regex.find_at(self.allocator, self.input, self.pos);
	if (catch err = m) return err~;
	usz start = m.start();
	usz end = m.end();
	if (end == start)
	{
		// Zero-length match: advance past the codepoint at the match start.
		// This avoids repeating zero-length matches that were found ahead of
		// the current scan position when find_at() searched forward.
		self.pos = start + 1;
		while (self.pos < self.input.len && (self.input[self.pos] & 0xC0) == 0x80) self.pos++;
	}
	else
	{
		self.pos = end;
	}
	return m;
}

// ============================================================
// Public compile API
// ============================================================

<*
 Compile a regex pattern.

 Returns a heap-owned `Regex*` that must be released with `Regex.free()`.
 Can fault with `INVALID_PATTERN` or `COMPILE_ERROR`.

 @param [&inout] allocator : "Allocator for the Regex object and its data"
 @param pattern : "The regex pattern string"
 @param flags : "Optional flags"
 @return "A compiled regex that must be released with Regex.free()."
 @return? INVALID_PATTERN, COMPILE_ERROR : "If parsing or compilation fails."
*>
fn Regex*? compile(Allocator allocator, String pattern, RegexFlags flags = FLAG_NONE)
{
	Regex* re = alloc::new(allocator, Regex);
	re.allocator = allocator;
	re.flags = flags;
	defer catch re.free();

	@pool()
	{
		RegexParseCtx ctx = {
			.pattern = pattern,
			.pos = 0,
			.next_group_id = 0,
			.regex_alloc = allocator,
			.flags = flags
		};

		RegexAstNode* ast = parse_expr(&ctx)!;
		if (ctx.pos != pattern.len) return INVALID_PATTERN~;
		re.group_count = ctx.next_group_id;

		RegexBuildCtx bctx = { .alloc = allocator };
		bctx.all_states.tinit(64);
		bctx.range_pool.tinit(16);

		RegexNfaFrag frag = build_nfa(&bctx, ast, flags)!;

		RegexNfaState* match_s = nfa_new(&bctx, STATE_MATCH);
		frag_patch(&frag, match_s);
		frag.out.free();

		usz n = bctx.all_states.size;
		RegexNfaState* state_arr = alloc::new_array(allocator, RegexNfaState, n);
		for (usz i = 0; i < n; i++)
		{
			state_arr[i] = *bctx.all_states.entries[i];
			state_arr[i].index = i;
		}
		for (usz i = 0; i < n; i++)
		{
			RegexNfaState* old = bctx.all_states.entries[i];
			if (old.out1 != null) state_arr[i].out1 = &state_arr[old.out1.index];
			if (old.out2 != null) state_arr[i].out2 = &state_arr[old.out2.index];
			if (old.lookaround_start != null) state_arr[i].lookaround_start = &state_arr[old.lookaround_start.index];
		}

		re.states = state_arr;
		re.state_count = n;
		re.start_idx = frag.start.index;

		for (usz i = 0; i < n; i++)
		{
			if (state_arr[i].kind == STATE_BACKREF) re.has_backref = true;
			if (state_arr[i].kind == STATE_ATOMIC) re.has_atomic = true;
			if (state_arr[i].kind == STATE_ANCHOR_GPOS) re.has_gpos = true;
			if (state_arr[i].kind == STATE_COND_GROUP) re.has_cond = true;
		}

		usz pool_size = bctx.range_pool.size;
		if (pool_size > 0)
		{
			RegexUnicodeRange* pool = alloc::new_array(allocator, RegexUnicodeRange, pool_size);
			for (usz i = 0; i < pool_size; i++) pool[i] = bctx.range_pool.entries[i];
			re.range_pool = pool;
			re.range_pool_count = pool_size;
			for (usz i = 0; i < n; i++)
			{
				if (state_arr[i].kind == STATE_CHARSET && state_arr[i].char_class.ranges_count > 0)
				{
					usz offset = (usz)(uptr)state_arr[i].char_class.ranges;
					state_arr[i].char_class.ranges = &pool[offset];
				}
			}
		}
		re.compute_start_filter();

		usz ng = ctx.named_groups_count;
		if (ng > 0)
		{
			re.named_groups = alloc::new_array(allocator, RegexNamedGroupEntry, ng);
			for (usz i = 0; i < ng; i++)
			{
				RegexNamedGroupEntry src = ctx.named_groups_buf[i];
				char* nbuf = alloc::new_array(allocator, char, src.name.len);
				mem::copy(nbuf, src.name.ptr, src.name.len);
				re.named_groups[i] = { .name = (String)nbuf[:src.name.len], .group_id = src.group_id };
			}
			re.named_group_count = ng;
		}

		for (usz i = 0; i < n; i++) alloc::free(allocator, bctx.all_states.entries[i]);
		bctx.all_states.free();
		bctx.range_pool.free();
	};
	return re;
}

<*
 Compile a regex using the temp allocator.

 The returned regex still owns heap state and should be freed with `Regex.free()` before
 the temp allocator is reset.

 @return "A compiled regex using the temp allocator."
*>
fn Regex* tcompile(String pattern, RegexFlags flags = FLAG_NONE) => compile(tmem, pattern, flags)!!;

<*
 Free all resources owned by the Regex.
*>
fn void Regex.free(&self)
{
	if (self.states != null)
	{
		alloc::free(self.allocator, self.states);
		self.states = null;
	}
	if (self.range_pool != null)
	{
		alloc::free(self.allocator, self.range_pool);
		self.range_pool = null;
	}
	if (self.start_filter.ranges != null)
	{
		alloc::free(self.allocator, self.start_filter.ranges);
		self.start_filter.ranges = null;
	}
	if (self.named_groups != null)
	{
		for (usz i = 0; i < self.named_group_count; i++)
		{
			alloc::free(self.allocator, self.named_groups[i].name.ptr);
		}
		alloc::free(self.allocator, self.named_groups);
		self.named_groups = null;
	}
	alloc::free(self.allocator, self);
}

// ============================================================
// Public find / match API
// ============================================================

<*
 Find the first match starting at start_pos.

 Returns a heap-owned `Match` that must be released with `Match.free()`.
 Returns `NO_MATCH` when no match is found.

 @param [&inout] allocator : "Allocator for Match.groups"
 @param start_pos : "The first byte position to search from; unanchored patterns may still advance beyond this point."
 @return "The first match at or after the requested start position."
 @return? NO_MATCH : "If no match is found."
*>
fn Match? Regex.find_at(&self, Allocator allocator, String input, usz start_pos = 0)
{
	bool anchored = self.state_count > 0
		&& (self.states[self.start_idx].kind == STATE_ANCHOR_ABSOLUTE_START
			|| self.states[self.start_idx].kind == STATE_ANCHOR_GPOS
			|| (self.states[self.start_idx].kind == STATE_ANCHOR_START
				&& (self.flags & FLAG_MULTILINE) == FLAG_NONE));
	usz pos = start_pos;
	while (pos <= input.len)
	{
		if (!anchored && self.has_start_filter)
		{
			while (pos < input.len && !self.start_filter.matches(codepoint_at(input, pos), (self.flags & FLAG_IGNORECASE) != FLAG_NONE))
			{
				pos = next_utf8_codepoint_start(input, pos, input.len);
			}
			if (pos > input.len || (pos == input.len && !anchored)) break;
		}
		Match? m = try_match_at(self, input, pos, allocator);
		if (try m) return m;
		if (anchored) break;
		if (pos >= input.len) break;
		pos = next_utf8_codepoint_start(input, pos, input.len);
	}
	return NO_MATCH~;
}

<*
 Find the first match in input.

 Returns a heap-owned `Match` that must be released with `Match.free()`.
 Returns `NO_MATCH` when no match is found.

 @param [&inout] allocator : "Allocator for Match.groups"
 @return "The first match in the input."
 @return? NO_MATCH : "If no match is found."
*>
fn Match? Regex.find(&self, Allocator allocator, String input) => self.find_at(allocator, input, 0);

<*
 Returns true if the pattern matches anywhere in input.

 @return "True if the pattern matches anywhere in the input."
*>
fn bool Regex.is_match(&self, String input)
{
	@pool()
	{
		return @ok(self.find_at(tmem, input));
	};
}

<*
 Returns a MatchIter over all non-overlapping matches.

 Each successful `next()` call yields a heap-owned `Match` that must be freed by the caller.

 @param [&inout] allocator : "Allocator for each Match returned by MatchIter.next()"
 @return "An iterator over all non-overlapping matches."
*>
fn MatchIter Regex.find_all(&self, Allocator allocator, String input)
{
	return { .regex = self, .input = input, .pos = 0, .allocator = allocator };
}

// ============================================================
// Replace
// ============================================================

fn usz? parse_replacement_group_index(String text) @local
{
	if (text.len == 0) return NO_MATCH~;
	usz value = 0;
	foreach (c : text)
	{
		if (c < '0' || c > '9') return NO_MATCH~;
		value = value * 10 + (usz)(c - '0');
	}
	return value;
}

fn void apply_replacement(DString* buf, String repl, Match* m)
{
	usz i = 0;
	while (i < repl.len)
	{
		char c = repl[i];
		if (c == '$' && i + 1 < repl.len)
		{
			char next = repl[i + 1];
			if (next == '$')
			{
				buf.append_char('$');
				i += 2;
				continue;
			}
			if (next == '&' || next == '0')
			{
				String? gs = m.group(0);
				if (try gs) { buf.append_string(gs); }
				i += 2;
				continue;
			}
			if (next >= '1' && next <= '9')
			{
				usz idx = (usz)(next - '0');
				String? gs = m.group(idx);
				if (try gs) { buf.append_string(gs); }
				i += 2;
				continue;
			}
			if (next == '{')
			{
				usz j = i + 2;
				while (j < repl.len && repl[j] != '}') j++;
				if (j < repl.len)
				{
					String name = repl[i + 2 : j - i - 2];
					String? gs;
					if (try idx = parse_replacement_group_index(name))
					{
						gs = m.group(idx);
					}
					else
					{
						gs = m.named_group(name);
					}
					if (try gs) { buf.append_string(gs); }
					i = j + 1;
					continue;
				}
			}
		}
		buf.append_char(c);
		i++;
	}
}

<*
 Replace the first match with replacement.

 Supported replacement escapes:
 - `$$` for a literal `$`
 - `$&` / `$0` for the full match
 - `$1`..`$9` for numbered captures
 - `${N}` for any numbered capture, including `${0}`
 - `${name}` for named captures

 Unknown or unmatched capture references expand to the empty string.
 Returns `NO_MATCH` if no match exists in the input.

 @return "A newly allocated string with the first match replaced."
 @return? NO_MATCH : "If no match exists in the input."
*>
fn String? Regex.replace(&self, Allocator allocator, String input, String replacement)
{
	Match? m = self.find(allocator, input);
	if (catch err = m) return err~;
	defer m.free();
	DString buf;
	buf.init(allocator, input.len + replacement.len);
	defer buf.free();
	buf.append_string(input[0 : m.start()]);
	apply_replacement(&buf, replacement, &m);
	buf.append_string(input[m.end() ..]);
	return buf.copy_str(allocator);
}

<*
 Replace all non-overlapping matches with replacement.

 Supported replacement escapes:
 - `$$` for a literal `$`
 - `$&` / `$0` for the full match
 - `$1`..`$9` for numbered captures
 - `${N}` for any numbered capture, including `${0}`
 - `${name}` for named captures

 Unknown or unmatched capture references expand to the empty string.
 Returns a newly allocated string. If there are no matches, the original input is copied.

 @return "A newly allocated string with all non-overlapping matches replaced."
*>
fn String? Regex.replace_all(&self, Allocator allocator, String input, String replacement)
{
	DString buf;
	buf.init(allocator, input.len);
	defer buf.free();
	usz pos = 0;
	while (pos <= input.len)
	{
		Match? m = self.find_at(allocator, input, pos);
		if (catch m) break;
		buf.append_string(input[pos : m.start() - pos]);
		apply_replacement(&buf, replacement, &m);
		usz match_start = m.start();
		usz new_pos = m.end();
		bool zero_length = match_start == new_pos;
		m.free();
		if (zero_length)
		{
			pos = new_pos;
			if (pos < input.len)
			{
				usz next_pos = next_utf8_codepoint_start(input, pos, input.len);
				buf.append_string(input[pos : next_pos - pos]);
				pos = next_pos;
			}
			else
			{
				pos = input.len + 1;
			}
		}
		else
		{
			pos = new_pos;
		}
	}
	if (pos <= input.len) buf.append_string(input[pos ..]);
	return buf.copy_str(allocator);
}

// ============================================================
// Parse / build contexts
// ============================================================

struct RegexParseCtx
{
	String    pattern;
	usz       pos;
	int       next_group_id;
	Allocator regex_alloc;
	RegexFlags flags;
	RegexNamedGroupEntry* named_groups_buf;
	usz named_groups_count;
	usz named_groups_cap;
}

struct RegexBuildCtx
{
	Allocator alloc;
	List{RegexNfaState*} all_states;
	List{RegexUnicodeRange} range_pool;
}
