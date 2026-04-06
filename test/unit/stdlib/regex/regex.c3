module regex_test @test;
import std::regex;
import std::io;

// Helper: compile pattern, assert is_match result, free
macro void @assert_match(String $pat, String $input)
{
	Regex* re = regex::compile(mem, $pat)!!;
	defer re.free();
	test::@check(re.is_match($input), "Expected /%s/ to match %s", $pat, $input);
}

macro void @assert_no_match(String $pat, String $input)
{
	Regex* re = regex::compile(mem, $pat)!!;
	defer re.free();
	test::@check(!re.is_match($input), "Expected /%s/ not to match %s", $pat, $input);
}

// ============================================================
// Basic literals
// ============================================================

fn void test_literal_match() @test
{
	Regex* re = regex::compile(mem, "hello")!!;
	defer re.free();
	assert(re.is_match("hello"));
	assert(re.is_match("say hello!"));
	assert(!re.is_match("hell"));
	assert(!re.is_match(""));
}

fn void test_literal_position() @test
{
	Regex* re = regex::compile(mem, "hello")!!;
	defer re.free();
	Match m = re.find(mem, "say hello!")!!;
	defer m.free();
	assert(m.start() == 4);
	assert(m.end()   == 9);
	assert(m.full()  == "hello");
}

fn void test_find_at_searches_forward_from_start() @test
{
	Regex* re = regex::compile(mem, "world")!!;
	defer re.free();
	Match m = re.find_at(mem, "hello world", 2)!!;
	defer m.free();
	assert(m.start() == 6);
	assert(m.full() == "world");
}

fn void test_find_with_nullable_prefix() @test
{
	Regex* re = regex::compile(mem, "a*bc")!!;
	defer re.free();
	Match m = re.find(mem, "xxbc")!!;
	defer m.free();
	assert(m.start() == 2);
	assert(m.full() == "bc");
}

fn void test_find_with_optional_prefix() @test
{
	Regex* re = regex::compile(mem, "(?:ab)?cd")!!;
	defer re.free();
	Match m = re.find(mem, "xcd")!!;
	defer m.free();
	assert(m.start() == 1);
	assert(m.full() == "cd");
}

fn void test_find_with_zero_width_prefix() @test
{
	Regex* re = regex::compile(mem, "(?=ab)ab")!!;
	defer re.free();
	Match m = re.find(mem, "zab")!!;
	defer m.free();
	assert(m.start() == 1);
	assert(m.full() == "ab");
}

fn void test_find_with_unicode_property_prefix() @test
{
	Regex* re = regex::compile(mem, "\\p{White_Space}+x")!!;
	defer re.free();
	Match m = re.find(mem, "a\u00A0x")!!;
	defer m.free();
	assert(m.start() == 1);
	assert(m.full() == "\u00A0x");
}

fn void test_dot() @test
{
	Regex* re = regex::compile(mem, "a.b")!!;
	defer re.free();
	assert( re.is_match("axb"));
	assert( re.is_match("a9b"));
	assert(!re.is_match("ab"));
	assert(!re.is_match("a\nb")); // dot does not match newline by default
}

fn void test_dot_dotall() @test
{
	Regex* re = regex::compile(mem, "a.b", regex::FLAG_DOTALL)!!;
	defer re.free();
	assert(re.is_match("a\nb")); // FLAG_DOTALL makes dot match \n
}

// ============================================================
// Character classes
// ============================================================

fn void test_charclass_basic() @test
{
	Regex* re = regex::compile(mem, "[abc]")!!;
	defer re.free();
	assert( re.is_match("a"));
	assert( re.is_match("b"));
	assert( re.is_match("c"));
	assert(!re.is_match("d"));
}

fn void test_charclass_range() @test
{
	Regex* re = regex::compile(mem, "[a-z]")!!;
	defer re.free();
	assert( re.is_match("m"));
	assert(!re.is_match("A"));
	assert(!re.is_match("0"));
}

fn void test_charclass_ascii_ignorecase() @test
{
	Regex* re = regex::compile(mem, "[A-Z]", regex::FLAG_IGNORECASE)!!;
	defer re.free();
	assert(re.is_match("A"));
	assert(re.is_match("a"));
	assert(!re.is_match("0"));
}

fn void test_charclass_negated() @test
{
	Regex* re = regex::compile(mem, "[^abc]")!!;
	defer re.free();
	assert(!re.is_match("a"));
	assert( re.is_match("d"));
}

fn void test_predefined_digit() @test
{
	Regex* re = regex::compile(mem, "\\d+")!!;
	defer re.free();
	assert( re.is_match("123"));
	assert( re.is_match("abc123"));
	assert(!re.is_match("abc"));
}

fn void test_predefined_word() @test
{
	Regex* re = regex::compile(mem, "\\w+")!!;
	defer re.free();
	assert( re.is_match("hello_123"));
	assert(!re.is_match("!@#"));
}

fn void test_predefined_word_unicode_latin1() @test
{
	Regex* re = regex::compile(mem, "\\w+")!!;
	defer re.free();
	assert(re.is_match("caf\u00E9"));
	assert(re.is_match("\u00E9clair"));
	assert(!re.is_match("\u2028"));
}

fn void test_predefined_space() @test
{
	Regex* re = regex::compile(mem, "\\s+")!!;
	defer re.free();
	assert( re.is_match("   "));
	assert( re.is_match("\t\n"));
	assert(!re.is_match("abc"));
}

fn void test_predefined_space_unicode_whitespace() @test
{
	Regex* re = regex::compile(mem, "\\s+")!!;
	defer re.free();
	assert(re.is_match("\u00A0"));   // NO-BREAK SPACE
	assert(re.is_match("\u2028"));   // LINE SEPARATOR
	assert(!re.is_match("\u00E9"));
}

fn void test_predefined_hspace_vspace_unicode_whitespace() @test
{
	Regex* h = regex::compile(mem, "\\h")!!;
	defer h.free();
	Regex* v = regex::compile(mem, "\\v")!!;
	defer v.free();

	assert(h.is_match("\u00A0"));
	assert(!h.is_match("\u2028"));
	assert(v.is_match("\u2028"));
	assert(!v.is_match("\u00A0"));
}

fn void test_predefined_inverse_in_class_matches_unicode() @test
{
	Regex* re = regex::compile(mem, "[\\D]")!!;
	defer re.free();
	assert(re.is_match("a"));
	assert(re.is_match("\u00E9"));
	assert(!re.is_match("5"));
}

fn void test_predefined_inverse_word_in_class_matches_unicode() @test
{
	Regex* re = regex::compile(mem, "[\\W]")!!;
	defer re.free();
	assert(re.is_match("!"));
	assert(!re.is_match("\u00E9"));
}

fn void test_predefined_inverse_space_in_class_matches_unicode() @test
{
	Regex* re = regex::compile(mem, "[\\S]")!!;
	defer re.free();
	assert(re.is_match("a"));
	assert(!re.is_match("\u00A0"));
	assert(!re.is_match("\u2028"));
}

fn void test_unicode_property_ascii() @test
{
	Regex* ascii_only = regex::compile(mem, "\\p{ASCII}+")!!;
	defer ascii_only.free();
	Regex* non_ascii = regex::compile(mem, "\\P{ASCII}+")!!;
	defer non_ascii.free();
	assert(ascii_only.is_match("AZ09_"));
	assert(!ascii_only.is_match("\u00E9"));
	assert(non_ascii.is_match("\u00E9"));
	assert(!non_ascii.is_match("A"));
}

fn void test_unicode_property_whitespace() @test
{
	Regex* ws = regex::compile(mem, "\\p{White_Space}+")!!;
	defer ws.free();
	Regex* not_ws = regex::compile(mem, "\\P{White_Space}+")!!;
	defer not_ws.free();
	assert(ws.is_match("\u205F"));   // MEDIUM MATHEMATICAL SPACE
	assert(ws.is_match("\u2029"));   // PARAGRAPH SEPARATOR
	assert(!ws.is_match("x"));
	assert(not_ws.is_match("x"));
	assert(!not_ws.is_match("\u205F"));
}

fn void test_unicode_property_whitespace_in_class() @test
{
	Regex* ws = regex::compile(mem, "[\\p{White_Space}]")!!;
	defer ws.free();
	Regex* not_ws = regex::compile(mem, "[\\P{White_Space}]")!!;
	defer not_ws.free();
	assert(ws.is_match("\u00A0"));
	assert(ws.is_match("\u2028"));
	assert(!ws.is_match("A"));
	assert(not_ws.is_match("A"));
	assert(!not_ws.is_match("\u00A0"));
}

fn void test_unicode_property_hspace_vspace() @test
{
	Regex* hs = regex::compile(mem, "\\p{Horiz_Space}+")!!;
	defer hs.free();
	Regex* vs = regex::compile(mem, "\\p{Vert_Space}+")!!;
	defer vs.free();
	assert(hs.is_match("\u3000"));
	assert(!hs.is_match("\u2028"));
	assert(vs.is_match("\u2028"));
	assert(!vs.is_match("\u3000"));
}

fn void test_unicode_property_word() @test
{
	Regex* word = regex::compile(mem, "\\p{Word}+")!!;
	defer word.free();
	assert(word.is_match("caf\u00E9"));
	assert(word.is_match("\u00B5"));
	assert(!word.is_match("!"));
}

fn void test_posix_alpha() @test
{
	Regex* re = regex::compile(mem, "[[:alpha:]]")!!;
	defer re.free();
	assert( re.is_match("a"));
	assert( re.is_match("Z"));
	assert(!re.is_match("0"));
	assert(!re.is_match("!"));
}

fn void test_posix_digit() @test
{
	Regex* re = regex::compile(mem, "[[:digit:]]")!!;
	defer re.free();
	assert( re.is_match("5"));
	assert(!re.is_match("a"));
}

fn void test_posix_xdigit() @test
{
	Regex* re = regex::compile(mem, "[[:xdigit:]]+")!!;
	defer re.free();
	assert( re.is_match("deadbeef"));
	assert( re.is_match("CAFE123"));
	assert(!re.is_match("ghij"));
}

fn void test_posix_space() @test
{
	Regex* re = regex::compile(mem, "[[:space:]]")!!;
	defer re.free();
	assert( re.is_match(" "));
	assert( re.is_match("\t"));
	assert(!re.is_match("a"));
}

fn void test_posix_upper_lower() @test
{
	Regex* re_u = regex::compile(mem, "[[:upper:]]")!!;
	defer re_u.free();
	Regex* re_l = regex::compile(mem, "[[:lower:]]")!!;
	defer re_l.free();
	assert( re_u.is_match("A"));
	assert(!re_u.is_match("a"));
	assert( re_l.is_match("a"));
	assert(!re_l.is_match("A"));
}

// ============================================================
// Anchors
// ============================================================

fn void test_anchor_start() @test
{
	Regex* re = regex::compile(mem, "^hello")!!;
	defer re.free();
	assert( re.is_match("hello world"));
	assert(!re.is_match("say hello"));
}

fn void test_anchor_end() @test
{
	Regex* re = regex::compile(mem, "world$")!!;
	defer re.free();
	assert( re.is_match("hello world"));
	assert(!re.is_match("world tour"));
}

fn void test_anchor_multiline() @test
{
	Regex* re = regex::compile(mem, "^foo", regex::FLAG_MULTILINE)!!;
	defer re.free();
	assert(re.is_match("bar\nfoo"));
	assert(re.is_match("foo"));
}

fn void test_word_boundary() @test
{
	Regex* re = regex::compile(mem, "\\bcat\\b")!!;
	defer re.free();
	assert( re.is_match("the cat sat"));
	assert(!re.is_match("catfish"));
	assert(!re.is_match("tomcat"));
}

fn void test_word_boundary_unicode_latin1() @test
{
	Regex* re = regex::compile(mem, "\\bcaf\u00E9\\b")!!;
	defer re.free();
	assert(re.is_match("caf\u00E9!"));
	assert(re.is_match("le caf\u00E9 noir"));
	assert(!re.is_match("decaf\u00E9s"));
}

// ============================================================
// Quantifiers
// ============================================================

fn void test_star() @test
{
	Regex* re = regex::compile(mem, "ab*c")!!;
	defer re.free();
	assert(re.is_match("ac"));
	assert(re.is_match("abc"));
	assert(re.is_match("abbbbc"));
}

fn void test_plus() @test
{
	Regex* re = regex::compile(mem, "ab+c")!!;
	defer re.free();
	assert( re.is_match("abc"));
	assert( re.is_match("abbbbc"));
	assert(!re.is_match("ac"));
}

fn void test_question() @test
{
	Regex* re = regex::compile(mem, "colou?r")!!;
	defer re.free();
	assert(re.is_match("color"));
	assert(re.is_match("colour"));
}

fn void test_repeat_exact() @test
{
	Regex* re = regex::compile(mem, "a{3}")!!;
	defer re.free();
	assert( re.is_match("aaa"));
	assert( re.is_match("xaaax"));
	assert(!re.is_match("aa"));
}

fn void test_repeat_zero_exact() @test
{
	Regex* re = regex::compile(mem, "a{0}b")!!;
	defer re.free();
	assert(re.is_match("b"));
	assert(!re.is_match(""));
}

fn void test_repeat_range() @test
{
	Regex* re = regex::compile(mem, "a{2,4}")!!;
	defer re.free();
	assert( re.is_match("aa"));
	assert( re.is_match("aaa"));
	assert( re.is_match("aaaa"));
}

fn void test_repeat_min() @test
{
	Regex* re = regex::compile(mem, "a{2,}")!!;
	defer re.free();
	assert( re.is_match("aa"));
	assert( re.is_match("aaaaaa"));
	assert(!re.is_match("a"));
}

fn void test_greedy_vs_lazy() @test
{
	Regex* greedy = regex::compile(mem, "<.+>")!!;
	defer greedy.free();
	Regex* lazy = regex::compile(mem, "<.+?>")!!;
	defer lazy.free();

	Match mg = greedy.find(mem, "<a><b>")!!;
	defer mg.free();
	Match ml = lazy.find(mem, "<a><b>")!!;
	defer ml.free();

	assert(mg.full() == "<a><b>");
	assert(ml.full() == "<a>");
}

fn void test_alternation_prefers_longest_match_at_same_start() @test
{
	Regex* re = regex::compile(mem, "sam|same")!!;
	defer re.free();
	Match m = re.find(mem, "same")!!;
	defer m.free();
	assert(m.full() == "same");
}

// ============================================================
// Alternation and groups
// ============================================================

fn void test_alternation() @test
{
	Regex* re = regex::compile(mem, "cat|dog")!!;
	defer re.free();
	assert( re.is_match("I have a cat"));
	assert( re.is_match("I have a dog"));
	assert(!re.is_match("I have a fish"));
}

fn void test_capturing_group() @test
{
	Regex* re = regex::compile(mem, "(\\w+)@(\\w+)")!!;
	defer re.free();
	Match m = re.find(mem, "user@example")!!;
	defer m.free();
	assert(m.group(1)!! == "user");
	assert(m.group(2)!! == "example");
}

fn void test_noncapturing_group() @test
{
	Regex* re = regex::compile(mem, "(?:ab)+")!!;
	defer re.free();
	assert( re.is_match("ababab"));
	assert(!re.is_match("aaa"));
}

fn void test_named_group() @test
{
	Regex* re = regex::compile(mem, "(?<year>\\d{4})-(?<month>\\d{2})-(?<day>\\d{2})")!!;
	defer re.free();
	Match m = re.find(mem, "date: 2024-03-15")!!;
	defer m.free();
	assert(m.named_group("year")!!  == "2024");
	assert(m.named_group("month")!! == "03");
	assert(m.named_group("day")!!   == "15");
}

// ============================================================
// Case-insensitive
// ============================================================

fn void test_ignorecase() @test
{
	Regex* re = regex::compile(mem, "hello", regex::FLAG_IGNORECASE)!!;
	defer re.free();
	assert(re.is_match("HELLO"));
	assert(re.is_match("Hello"));
	assert(re.is_match("hElLo"));
}

fn void test_inline_ignorecase_is_parse_only_compatibility_syntax() @test
{
	Regex* re = regex::compile(mem, "ab(?i)cd")!!;
	defer re.free();
	assert(re.is_match("abcd"));
	assert(!re.is_match("abCD"));
}

fn void test_inline_multiline_is_parse_only_compatibility_syntax() @test
{
	Regex* re = regex::compile(mem, "(?m)^foo")!!;
	defer re.free();
	assert(!re.is_match("bar\nfoo"));
	assert(re.is_match("foo"));
}

// ============================================================
// Lookahead / Lookbehind
// ============================================================

fn void test_lookahead_pos() @test
{
	Regex* re = regex::compile(mem, "\\d+(?= dollars)")!!;
	defer re.free();
	Match m = re.find(mem, "100 dollars")!!;
	defer m.free();
	assert(m.full() == "100");
	assert(!re.is_match("100 euros"));
}

fn void test_lookahead_with_word_boundary() @test
{
	Regex* re = regex::compile(mem, "(?=\\bfoo)foo")!!;
	defer re.free();
	assert(re.is_match("foo"));
	assert(!re.is_match("xfoo"));
}

fn void test_lookahead_neg() @test
{
	Regex* re = regex::compile(mem, "\\d+(?! dollars)")!!;
	defer re.free();
	assert(re.is_match("100 euros"));
}

fn void test_lookbehind_pos() @test
{
	Regex* re = regex::compile(mem, "(?<=\\$)\\d+")!!;
	defer re.free();
	Match m = re.find(mem, "price: $42")!!;
	defer m.free();
	assert(m.full() == "42");
}

fn void test_lookbehind_pos_utf8_fixed_width() @test
{
	Regex* re = regex::compile(mem, "(?<=\u00E9)a")!!;
	defer re.free();
	assert(re.is_match("\u00E9a"));
	assert(!re.is_match("ea"));
}

fn void test_lookbehind_neg() @test
{
	Regex* re = regex::compile(mem, "(?<!\\$)\\d+")!!;
	defer re.free();
	assert(re.is_match("42 items"));
}

// ============================================================
// Unicode
// ============================================================

fn void test_unicode_literal() @test
{
	// é = U+00E9 (2-byte UTF-8: 0xC3 0xA9)
	Regex* re = regex::compile(mem, "\u00E9")!!;
	defer re.free();
	assert(re.is_match("caf\u00E9"));
	assert(!re.is_match("cafe"));
}

fn void test_unicode_escape_pattern() @test
{
	Regex* re = regex::compile(mem, "\\u00E9")!!;
	defer re.free();
	assert(re.is_match("caf\u00E9"));
}

// ============================================================
// find_all
// ============================================================

fn void test_find_all() @test
{
	Regex* re = regex::compile(mem, "\\d+")!!;
	defer re.free();
	MatchIter it = re.find_all(mem, "abc 123 def 456 ghi 789");
	String[3] expected = { "123", "456", "789" };
	int count = 0;
	while (try m = it.next())
	{
		assert(count < 3);
		assert(m.full() == expected[count]);
		m.free();
		count++;
	}
	assert(count == 3);
}

fn void test_find_all_zero_length() @test
{
	// a* can match empty string; find_all must not loop infinitely
	Regex* re = regex::compile(mem, "a*")!!;
	defer re.free();
	MatchIter it = re.find_all(mem, "bab");
	int count = 0;
	while (try m = it.next())
	{
		m.free();
		count++;
		if (count > 20) break; // guard
	}
	assert(count <= 10); // should finish
}

fn void test_find_all_zero_length_word_boundaries_no_duplicates() @test
{
	Regex* re = regex::compile(mem, "\\b")!!;
	defer re.free();
	MatchIter it = re.find_all(mem, "ab cd");

	Match m1 = it.next()!!;
	defer m1.free();
	assert(m1.start() == 0);
	assert(m1.full() == "");

	Match m2 = it.next()!!;
	defer m2.free();
	assert(m2.start() == 2);
	assert(m2.full() == "");

	Match m3 = it.next()!!;
	defer m3.free();
	assert(m3.start() == 3);
	assert(m3.full() == "");

	Match m4 = it.next()!!;
	defer m4.free();
	assert(m4.start() == 5);
	assert(m4.full() == "");

	assert(@catch(it.next()) == regex::NO_MATCH);
}

// ============================================================
// replace / replace_all
// ============================================================

fn void test_replace() @test
{
	Regex* re = regex::compile(mem, "world")!!;
	defer re.free();
	String result = re.replace(mem, "hello world", "C3")!!;
	defer alloc::free(mem, result.ptr);
	assert(result == "hello C3");
}

fn void test_replace_all() @test
{
	Regex* re = regex::compile(mem, "\\d+")!!;
	defer re.free();
	String result = re.replace_all(mem, "a1b2c3", "X")!!;
	defer alloc::free(mem, result.ptr);
	assert(result == "aXbXcX");
}

fn void test_replace_all_zero_length_boundaries() @test
{
	Regex* re = regex::compile(mem, "\\b")!!;
	defer re.free();
	String result = re.replace_all(mem, "ab cd", "|")!!;
	defer alloc::free(mem, result.ptr);
	assert(result == "|ab| |cd|");
}

fn void test_replace_group_ref() @test
{
	Regex* re = regex::compile(mem, "(\\w+)@(\\w+)")!!;
	defer re.free();
	String result = re.replace(mem, "user@example", "$2/$1")!!;
	defer alloc::free(mem, result.ptr);
	assert(result == "example/user");
}

fn void test_replace_named_group() @test
{
	Regex* re = regex::compile(mem, "(?<first>\\w+) (?<last>\\w+)")!!;
	defer re.free();
	String result = re.replace(mem, "John Doe", "${last}, ${first}")!!;
	defer alloc::free(mem, result.ptr);
	assert(result == "Doe, John");
}

fn void test_replace_literal_dollar_escape() @test
{
	Regex* re = regex::compile(mem, "world")!!;
	defer re.free();
	String result = re.replace(mem, "hello world", "$$$0 $$")!!;
	defer alloc::free(mem, result.ptr);
	assert(result == "hello $world $");
}

fn void test_replace_braced_numeric_group_refs() @test
{
	Regex* re = regex::compile(mem, "(\\w+)@(\\w+)")!!;
	defer re.free();
	String result = re.replace(mem, "user@example", "${2}/${1}/${0}")!!;
	defer alloc::free(mem, result.ptr);
	assert(result == "example/user/user@example");
}

fn void test_replace_all_literal_dollar_escape() @test
{
	Regex* re = regex::compile(mem, "\\d+")!!;
	defer re.free();
	String result = re.replace_all(mem, "a1b2", "$$$0")!!;
	defer alloc::free(mem, result.ptr);
	assert(result == "a$1b$2");
}

fn void test_replace_unknown_or_unmatched_groups_expand_empty() @test
{
	Regex* re = regex::compile(mem, "(a)?b")!!;
	defer re.free();
	String result = re.replace(mem, "b", "<$1><${9}><${missing}>")!!;
	defer alloc::free(mem, result.ptr);
	assert(result == "<><><>");
}

fn void test_match_group_faults_for_missing_or_unmatched_groups() @test
{
	Regex* re = regex::compile(mem, "(a)?b")!!;
	defer re.free();
	Match m = re.find(mem, "b")!!;
	defer m.free();
	assert(@catch(m.group(1)) == regex::GROUP_NOT_FOUND);
	assert(@catch(m.group(99)) == regex::GROUP_NOT_FOUND);
	assert(@catch(m.named_group("missing")) == regex::GROUP_NOT_FOUND);
}

// ============================================================
// Error cases
// ============================================================

fn void test_invalid_unclosed_paren() @test
{
	assert(@catch(regex::compile(mem, "(abc")) == regex::INVALID_PATTERN);
}

fn void test_invalid_inverted_range() @test
{
	assert(@catch(regex::compile(mem, "[z-a]")) == regex::INVALID_PATTERN);
}

fn void test_invalid_repeat_too_large() @test
{
	assert(@catch(regex::compile(mem, "a{5000}")));
}

fn void test_invalid_quantified_anchor() @test
{
	assert(@catch(regex::compile(mem, "^*")) == regex::INVALID_PATTERN);
}

fn void test_invalid_quantified_lookaround() @test
{
	assert(@catch(regex::compile(mem, "(?=a)+")) == regex::INVALID_PATTERN);
	assert(@catch(regex::compile(mem, "(?<!a){2}")) == regex::INVALID_PATTERN);
}

fn void test_invalid_unknown_unicode_property() @test
{
	assert(@catch(regex::compile(mem, "\\p{Letter}")) == regex::INVALID_PATTERN);
	assert(@catch(regex::compile(mem, "[\\P{Letter}]")) == regex::INVALID_PATTERN);
}

fn void test_invalid_scoped_inline_flag_group() @test
{
	assert(@catch(regex::compile(mem, "(?i:foo)")) == regex::INVALID_PATTERN);
	assert(@catch(regex::compile(mem, "(?x: a b c )")) == regex::INVALID_PATTERN);
}

// ============================================================
// Tier 1: new features
// ============================================================

fn void test_comment_group() @test
{
	// (?#...) should be ignored entirely
	Regex* re = regex::compile(mem, "hel(?#this is a comment)lo")!!;
	defer re.free();
	assert(re.is_match("hello"));
	assert(!re.is_match("hel"));
}

fn void test_absolute_start_anchor() @test
{
	Regex* re = regex::compile(mem, "\\Ahello")!!;
	defer re.free();
	assert( re.is_match("hello world"));
	assert(!re.is_match("say hello"));
}

fn void test_absolute_end_anchor() @test
{
	Regex* re = regex::compile(mem, "world\\z")!!;
	defer re.free();
	assert( re.is_match("hello world"));
	assert(!re.is_match("world tour"));
}

fn void test_absolute_end_anchor_z_capital() @test
{
	// \Z matches at end of string or just before a final \n
	Regex* re = regex::compile(mem, "world\\Z")!!;
	defer re.free();
	assert( re.is_match("hello world"));
	assert( re.is_match("hello world\n"));  // before final \n
	assert(!re.is_match("world\ntour"));
}

fn void test_absolute_anchor_not_multiline() @test
{
	// \A should NOT match after a newline, even with FLAG_MULTILINE
	Regex* re = regex::compile(mem, "\\Afoo", regex::FLAG_MULTILINE)!!;
	defer re.free();
	assert( re.is_match("foo"));
	assert(!re.is_match("bar\nfoo"));  // \A is not multiline-aware
}

fn void test_ungreedy_flag() @test
{
	// FLAG_UNGREEDY makes * lazy by default
	Regex* greedy = regex::compile(mem, "<.+>")!!;
	defer greedy.free();
	Regex* lazy   = regex::compile(mem, "<.+>", regex::FLAG_UNGREEDY)!!;
	defer lazy.free();

	Match mg = greedy.find(mem, "<a><b>")!!;
	defer mg.free();
	Match ml = lazy.find(mem, "<a><b>")!!;
	defer ml.free();

	assert(mg.full() == "<a><b>");
	assert(ml.full() == "<a>");
}

fn void test_ungreedy_suffix_restores_greedy() @test
{
	// With FLAG_UNGREEDY, adding ? suffix makes the quantifier greedy
	Regex* re = regex::compile(mem, "<.+?>", regex::FLAG_UNGREEDY)!!;
	defer re.free();
	Match m = re.find(mem, "<a><b>")!!;
	defer m.free();
	assert(m.full() == "<a><b>");
}

fn void test_inline_ungreedy_flag() @test
{
	// (?U) sets ungreedy mode inline
	Regex* re = regex::compile(mem, "(?U)<.+>")!!;
	defer re.free();
	Match m = re.find(mem, "<a><b>")!!;
	defer m.free();
	assert(m.full() == "<a>");
}

fn void test_freespacing_mode() @test
{
	// FLAG_EXTENDED: whitespace and # comments ignored
	Regex* re = regex::compile(mem, "hel  lo  # match hello", regex::FLAG_EXTENDED)!!;
	defer re.free();
	assert( re.is_match("hello"));
	assert(!re.is_match("hel  lo"));
}

fn void test_freespacing_inline() @test
{
	// (?x) inline
	Regex* re = regex::compile(mem, "(?x) \\d+ # one or more digits")!!;
	defer re.free();
	assert( re.is_match("123"));
	assert(!re.is_match("abc"));
}

fn void test_linebreak_R() @test
{
	// \R matches any Unicode line break
	Regex* re = regex::compile(mem, "a\\Rb")!!;
	defer re.free();
	assert(re.is_match("a\nb"));
	assert(re.is_match("a\rb"));
	assert(re.is_match("a\r\nb"));  // CRLF counts as one \R
	assert(re.is_match("a\vb"));
	assert(re.is_match("a\fb"));
	assert(!re.is_match("ab"));
}

// ============================================================
// Tier 2: new features
// ============================================================

fn void test_hspace() @test
{
	Regex* re = regex::compile(mem, "\\h+")!!;
	defer re.free();
	assert( re.is_match(" \t "));
	assert( re.is_match("a b"));   // contains space
	assert(!re.is_match("abc"));
	assert(!re.is_match("\n"));    // \n is NOT horizontal whitespace
}

fn void test_hspace_inv() @test
{
	Regex* re = regex::compile(mem, "\\H+")!!;
	defer re.free();
	assert( re.is_match("abc"));
	assert(!re.is_match(" \t"));
}

fn void test_vspace_class() @test
{
	// \v as a CLASS (vertical whitespace), not literal VT
	Regex* re = regex::compile(mem, "\\v")!!;
	defer re.free();
	assert(re.is_match("\n"));
	assert(re.is_match("\r"));
	assert(re.is_match("\v"));  // VT is still in the class
	assert(re.is_match("\f"));
	assert(!re.is_match("a"));
}

fn void test_vspace_inv() @test
{
	Regex* re = regex::compile(mem, "\\V+")!!;
	defer re.free();
	assert( re.is_match("abc"));
	assert(!re.is_match("\n\r\v\f"));
}

fn void test_non_newline() @test
{
	// \N = non-newline, ignores FLAG_DOTALL
	Regex* re_dot  = regex::compile(mem, "a.b",  regex::FLAG_DOTALL)!!;
	defer re_dot.free();
	Regex* re_N    = regex::compile(mem, "a\\Nb")!!;
	defer re_N.free();

	assert( re_dot.is_match("a\nb"));  // . with DOTALL matches \n
	assert(!re_N.is_match("a\nb"));    // \N never matches \n
	assert( re_N.is_match("axb"));
}

fn void test_keep() @test
{
	// \K resets the match start
	Regex* re = regex::compile(mem, "foo\\Kbar")!!;
	defer re.free();
	Match m = re.find(mem, "foobar")!!;
	defer m.free();
	assert(m.full() == "bar");     // match text is only "bar"
	assert(m.start() == 3);        // starts after "foo"
}

fn void test_keep_in_find_all() @test
{
	Regex* re = regex::compile(mem, "\\w+\\K\\s+")!!;
	defer re.free();
	// Match the whitespace after a word, but report only the whitespace
	Match m = re.find(mem, "hello   world")!!;
	defer m.free();
	assert(m.full() == "   ");
}

fn void test_keep_preserves_captures() @test
{
	Regex* re = regex::compile(mem, "ab(c)\\Kde")!!;
	defer re.free();
	Match m = re.find(mem, "abcde")!!;
	defer m.free();
	assert(m.full() == "de");
	assert(m.group(1)!! == "c");
}

fn void test_unicode_icase_literal() @test
{
	// Latin-1 Supplement case folding
	Regex* re = regex::compile(mem, "caf\u00E9", regex::FLAG_IGNORECASE)!!;
	defer re.free();
	assert(re.is_match("caf\u00E9"));  // lowercase é
	assert(re.is_match("CAF\u00C9"));  // uppercase É (U+00C9)
	assert(re.is_match("Caf\u00E9"));  // mixed
}

fn void test_unicode_icase_literal_greek() @test
{
	Regex* re = regex::compile(mem, "\u03C3", regex::FLAG_IGNORECASE)!!;
	defer re.free();
	assert(re.is_match("\u03A3"));  // Σ
	assert(re.is_match("\u03C3"));  // σ
	assert(re.is_match("\u03C2"));  // ς
}

fn void test_unicode_icase_literal_cyrillic() @test
{
	Regex* re = regex::compile(mem, "\u0436", regex::FLAG_IGNORECASE)!!;
	defer re.free();
	assert(re.is_match("\u0416"));  // Ж
	assert(re.is_match("\u0436"));  // ж
	assert(!re.is_match("x"));
}

fn void test_unicode_icase_literal_latin_extended() @test
{
	Regex* re = regex::compile(mem, "\u00FF", regex::FLAG_IGNORECASE)!!;
	defer re.free();
	assert(re.is_match("\u0178"));  // Ÿ
	assert(re.is_match("\u00FF"));  // ÿ
}

fn void test_unicode_icase_charclass() @test
{
	// [À-Ö] with IGNORECASE should match both upper and lower
	Regex* re = regex::compile(mem, "[\u00C0-\u00D6]", regex::FLAG_IGNORECASE)!!;
	defer re.free();
	assert(re.is_match("\u00C0"));  // À (uppercase, in range)
	assert(re.is_match("\u00E0"));  // à (lowercase, should match via icase)
	assert(re.is_match("\u00D6"));  // Ö
	assert(re.is_match("\u00F6"));  // ö
}

fn void test_unicode_icase_charclass_greek_cyrillic() @test
{
	Regex* greek = regex::compile(mem, "[\u03A3]", regex::FLAG_IGNORECASE)!!;
	defer greek.free();
	Regex* cyrillic = regex::compile(mem, "[\u0416]", regex::FLAG_IGNORECASE)!!;
	defer cyrillic.free();
	assert(greek.is_match("\u03C3"));
	assert(greek.is_match("\u03C2"));
	assert(cyrillic.is_match("\u0436"));
}

fn void test_variable_lookbehind() @test
{
	// Variable-width positive lookbehind
	Regex* re = regex::compile(mem, "(?<=foo+)bar")!!;
	defer re.free();
	assert( re.is_match("foobar"));
	assert( re.is_match("fooobar"));
	assert(!re.is_match("bar"));
	assert(!re.is_match("xbar"));
}

fn void test_variable_lookbehind_neg() @test
{
	// Variable-width negative lookbehind
	Regex* re = regex::compile(mem, "(?<!foo+)bar")!!;
	defer re.free();
	assert( re.is_match("xbar"));
	assert( re.is_match("bar"));
	assert(!re.is_match("foobar"));
	assert(!re.is_match("fooobar"));
}

fn void test_variable_lookbehind_scans_beyond_1000_bytes() @test
{
	Regex* re = regex::compile(mem, "(?<=\u00E9{500,}c)b")!!;
	defer re.free();
	DString input_builder = dstring::new_with_capacity(mem, 1002);
	defer input_builder.free();
	for (int i = 0; i < 500; i++) input_builder.append("\u00E9");
	input_builder.append("cb");
	String input = input_builder.str_view();
	Match m = re.find(mem, input)!!;
	defer m.free();
	assert(m.start() == 1001);
	assert(m.full() == "b");
}

fn void test_variable_lookbehind_neg_scans_beyond_1000_bytes() @test
{
	Regex* re = regex::compile(mem, "(?<!\u00E9{500,}c)b")!!;
	defer re.free();
	DString input_builder = dstring::new_with_capacity(mem, 1002);
	defer input_builder.free();
	for (int i = 0; i < 500; i++) input_builder.append("\u00E9");
	input_builder.append("cb");
	String input = input_builder.str_view();
	assert(!re.is_match(input));
}

fn void test_variable_lookbehind_scans_beyond_1000_bytes_in_backtracking_engine() @test
{
	Regex* re = regex::compile(mem, "(?<=\u00E9{500,}c)(b)\\1")!!;
	defer re.free();
	DString input_builder = dstring::new_with_capacity(mem, 1003);
	defer input_builder.free();
	for (int i = 0; i < 500; i++) input_builder.append("\u00E9");
	input_builder.append("cbb");
	String input = input_builder.str_view();
	Match m = re.find(mem, input)!!;
	defer m.free();
	assert(m.start() == 1001);
	assert(m.full() == "bb");
	assert(m.group(1)!! == "b");
}

fn void test_variable_lookbehind_neg_scans_beyond_1000_bytes_in_backtracking_engine() @test
{
	Regex* re = regex::compile(mem, "(?<!\u00E9{500,}c)(b)\\1")!!;
	defer re.free();
	DString input_builder = dstring::new_with_capacity(mem, 1003);
	defer input_builder.free();
	for (int i = 0; i < 500; i++) input_builder.append("\u00E9");
	input_builder.append("cbb");
	String input = input_builder.str_view();
	assert(!re.is_match(input));
}

// ============================================================
// Performance: no catastrophic backtracking
// ============================================================

fn void test_no_catastrophic_backtracking() @test
{
	// (a+)+b against 30 a's — exponential for backtracking engines,
	// linear for Thompson NFA
	Regex* re = regex::compile(mem, "(a+)+b")!!;
	defer re.free();
	assert(!re.is_match("aaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"));
}

// ============================================================
// Compile-test: all supported syntax compiles without error
// ============================================================

fn void test_syntax_coverage() @test
{
	String[] patterns = {
		"abc",
		"a|b|c",
		"(ab)+",
		"(?:ab){3}",
		"(?<name>\\d+)",
		"[a-zA-Z0-9_]",
		"[[:alpha:]]+",
		"[[:alnum:]_]+",
		"\\d\\w\\s\\D\\W\\S",
		"\\h\\H\\v\\V\\N",
		"\\p{ASCII}+",
		"\\p{Word}+",
		"\\P{White_Space}+",
		"[\\p{Horiz_Space}\\P{Vert_Space}]",
		"foo\\Kbar",
		"(?<=\\w+)\\d+",   // variable-width lookbehind
		"^start",
		"end$",
		"\\bword\\b",
		"a*b+c?",
		"a{2,5}",
		"a{3,}",
		".+",
		"(?i)hello",
		"(?m)^foo",
		"(?s)a.b",
		"(?U)<.+>",
		"(?x) hel lo # match hello",
		"(?#comment)hello",
		"\\Astart",
		"end\\z",
		"end\\Z",
		"a\\Rb",
		"(?=\\d)",
		"(?!\\d)",
		"(?<=\\s)",
		"(?<!\\s)",
		"\\u0041",   // \uXXXX
		"(?>a|ab)",  // atomic group
		"(\\w+) \\1", // backreference
		"(?<x>\\w+) \\k<x>", // named backreference
		"a*+b",      // possessive star
		"a++b",      // possessive plus
		"a?+b",      // possessive quest
		"\\e",      // ESC char
		"\\cA",     // control char
		"\\x{41}",  // braced hex
		"\\Q.*\\E", // literal quoting
		"(?|(a)|(b))", // branch reset
		"\\G\\w+", // \G anchor
	};
	foreach (pat : patterns)
	{
		Regex*? re = regex::compile(mem, pat);
		if (!@ok(re))
		{
			io::printfn("Pattern failed to compile: %s", pat);
		}
		test::@check(@ok(re), "Pattern %s failed to compile", pat);
		if (try re) re.free();
	}
}
// ============================================================
// Tier 3: Backreferences, atomic groups, possessive quantifiers
// ============================================================

fn void test_backref_simple() @test
{
	// \1 matches same text as group 1
	Regex* re = regex::tcompile("(\\w+) \\1");
	test::@check(re.is_match("hello hello"));
	test::@check(!re.is_match("hello world"));
}

fn void test_backref_case_insensitive() @test
{
	Regex* re = regex::compile(mem, "(\\w+) \\1", regex::FLAG_IGNORECASE)!!;
	defer re.free();
	test::@check(re.is_match("Hello hello"));
	test::@check(re.is_match("TEST test"));
	test::@check(!re.is_match("foo bar"));
}

fn void test_backref_named() @test
{
	// \k<name> matches the same text as the named group
	Regex* re = regex::tcompile("(?<word>\\w+) \\k<word>");
	test::@check(re.is_match("cat cat"));
	test::@check(!re.is_match("cat dog"));
}

fn void test_backref_capture() @test
{
	@pool()
	{
		Regex* re = regex::tcompile("([a-z]+)\\1");
		Match? m = re.find(tmem, "abcabc");
		test::@check(@ok(m));
		if (try m) {
			test::@check(m.full() == "abcabc");
			m.free();
		}
	};
}

fn void test_atomic_group_basic() @test
{
	// (?>a|ab) atomically matches 'a' — will not backtrack to try 'ab'
	Regex* re = regex::tcompile("(?>a|ab)c");
	// 'a' consumed atomically, then 'c' fails because next char is 'b'
	test::@check(!re.is_match("abc"));
	// But straight 'ac' works
	test::@check(re.is_match("ac"));
}

fn void test_atomic_group_greedy_commit() @test
{
	// (?>.*) consumes all — continuation always fails
	Regex* re = regex::tcompile("(?>.*)x");
	test::@check(!re.is_match("abcx"));
}

fn void test_atomic_vs_non_atomic_alternation() @test
{
	Regex* non_atomic = regex::compile(mem, "(a|ab)c")!!;
	defer non_atomic.free();
	Regex* atomic = regex::compile(mem, "(?>a|ab)c")!!;
	defer atomic.free();
	assert(non_atomic.is_match("abc"));
	assert(!atomic.is_match("abc"));
}

fn void test_possessive_star() @test
{
	// a*+b: possessive star, no backtrack
	Regex* re = regex::tcompile("a*+b");
	test::@check(re.is_match("aaab"));
	// Without possessive: "aaa" consumed, 'b' matches; with possessive same result here
	test::@check(!re.is_match("aaac"));
	// The possessive prevents backtracking: a*+ eats all 'a's, then 'b' must follow directly
	test::@check(!re.is_match("aaaa"));  // no 'b' at end
}

fn void test_possessive_plus() @test
{
	// \\w++ consumes entire word, can't backtrack
	Regex* re = regex::tcompile("\\w++\\d");
	// \w++ eats "abc3", then needs a \d after — but all consumed
	test::@check(!re.is_match("abc3"));
}

fn void test_possessive_quest() @test
{
	// a?+b — possessive optional
	Regex* re = regex::tcompile("a?+b");
	test::@check(re.is_match("ab"));
	test::@check(re.is_match("b"));
	// a?+ consumed 'a' possessively; next must be 'b'
	test::@check(!re.is_match("a"));
}

fn void test_possessive_prevents_suffix_backtracking() @test
{
	Regex* greedy = regex::compile(mem, "a*ab")!!;
	defer greedy.free();
	Regex* possessive = regex::compile(mem, "a*+ab")!!;
	defer possessive.free();
	assert(greedy.is_match("aaab"));
	assert(!possessive.is_match("aaab"));
}

fn void test_backref_in_find() @test
{
	@pool()
	{
		Regex* re = regex::tcompile("(\\d+)-\\1");
		Match? m = re.find(tmem, "price: 42-42 end");
		test::@check(@ok(m));
		if (try m) {
			test::@check(m.full() == "42-42");
			m.free();
		}
	};
}

fn void test_backtracking_engine_handles_long_linear_chain() @test
{
	DString pattern_builder = dstring::new_with_capacity(mem, 1028);
	defer pattern_builder.free();
	pattern_builder.append("\\G");
	for (int i = 0; i < 1024; i++) pattern_builder.append("a");
	pattern_builder.append("\\z");

	DString input_builder = dstring::new_with_capacity(mem, 1024);
	defer input_builder.free();
	input_builder.append_repeat('a', 1024);

	Regex* re = regex::compile(mem, pattern_builder.str_view())!!;
	defer re.free();
	assert(re.is_match(input_builder.str_view()));
}

fn void test_backtracking_engine_handles_many_choice_points() @test
{
	const int OPTIONAL_COUNT = 24;
	const int SUFFIX_COUNT = 12;

	DString pattern_builder = dstring::new_with_capacity(mem, 80);
	defer pattern_builder.free();
	pattern_builder.append("\\G");
	for (int i = 0; i < OPTIONAL_COUNT; i++) pattern_builder.append("a?");
	for (int i = 0; i < SUFFIX_COUNT; i++) pattern_builder.append("a");
	pattern_builder.append("b\\z");

	DString input_builder = dstring::new_with_capacity(mem, OPTIONAL_COUNT + 1);
	defer input_builder.free();
	input_builder.append_repeat('a', OPTIONAL_COUNT);
	input_builder.append("b");

	Regex* re = regex::compile(mem, pattern_builder.str_view())!!;
	defer re.free();
	assert(re.is_match(input_builder.str_view()));
}

fn void test_backtracking_engine_handles_long_atomic_chain() @test
{
	DString pattern_builder = dstring::new_with_capacity(mem, 3076);
	defer pattern_builder.free();
	pattern_builder.append("\\G");
	for (int i = 0; i < 512; i++) pattern_builder.append("(?>a)");
	pattern_builder.append("\\z");

	DString input_builder = dstring::new_with_capacity(mem, 512);
	defer input_builder.free();
	input_builder.append_repeat('a', 512);

	Regex* re = regex::compile(mem, pattern_builder.str_view())!!;
	defer re.free();
	assert(re.is_match(input_builder.str_view()));
}

fn void test_backtracking_engine_handles_long_conditional_chain() @test
{
	DString pattern_builder = dstring::new_with_capacity(mem, 2052);
	defer pattern_builder.free();
	pattern_builder.append("\\G");
	for (int i = 0; i < 128; i++) pattern_builder.append("(a)?(?(1)a|b)");
	pattern_builder.append("\\z");

	DString input_builder = dstring::new_with_capacity(mem, 256);
	defer input_builder.free();
	for (int i = 0; i < 128; i++) input_builder.append("aa");

	Regex* re = regex::compile(mem, pattern_builder.str_view())!!;
	defer re.free();
	assert(re.is_match(input_builder.str_view()));
}

// ============================================================
// Tier 4: Escape sequences, literal quoting, branch reset, conditionals, \G
// ============================================================

fn void test_escape_esc() @test
{
	// \e = ESC (0x1B)
	Regex* re = regex::tcompile("\\e");
	test::@check(re.is_match("\x1b"));
	test::@check(!re.is_match("e"));
}

fn void test_escape_control_char() @test
{
	// \cA = chr(1), \cG = BEL (chr(7))
	Regex* re = regex::tcompile("\\cA");
	test::@check(re.is_match("\x01"));
	test::@check(!re.is_match("A"));
}

fn void test_escape_hex_braced() @test
{
	// \x{41} = 'A', \x{263A} = smiley U+263A
	Regex* re = regex::tcompile("\\x{41}");
	test::@check(re.is_match("A"));
	test::@check(!re.is_match("B"));
}

fn void test_escape_hex_two_digit() @test
{
	// \x41 = 'A' (2-digit hex)
	Regex* re = regex::tcompile("\\x41");
	test::@check(re.is_match("A"));
	test::@check(!re.is_match("B"));
}

fn void test_literal_quoting_basic() @test
{
	// \Q...\E treats ., *, + etc as literals
	Regex* re = regex::tcompile("\\Q.*+?\\E");
	test::@check(re.is_match(".*+?"));
	test::@check(!re.is_match("abcd"));
}

fn void test_literal_quoting_in_pattern() @test
{
	// Pattern before and after \Q...\E should still work as regex
	Regex* re = regex::tcompile("\\d+\\Q.\\E\\d+");
	test::@check(re.is_match("123.456"));
	test::@check(!re.is_match("123x456"));
}

fn void test_literal_quoting_empty() @test
{
	// \Q\E is empty — should match adjacent context
	Regex* re = regex::tcompile("ab\\Q\\Ecd");
	test::@check(re.is_match("abcd"));
}

fn void test_branch_reset_basic() @test
{
	// (?|(\d+)|(\w+)) — both branches use group 1
	@pool()
	{
		Regex* re = regex::tcompile("(?|(\\d+)|(\\w+))");
		Match? m1 = re.find(tmem, "123");
		test::@check(@ok(m1));
		if (try m1) {
			// Group 1 should capture the digits
			String? g = m1.group(1);
			test::@check(@ok(g));
			if (try g) test::@check(g == "123");
			m1.free();
		}
		Match? m2 = re.find(tmem, "abc");
		test::@check(@ok(m2));
		if (try m2) {
			String? g = m2.group(1);
			test::@check(@ok(g));
			if (try g) test::@check(g == "abc");
			m2.free();
		}
	};
}

fn void test_branch_reset_backref_reuses_group_number() @test
{
	Regex* re = regex::compile(mem, "(?|(a)|(b))\\1")!!;
	defer re.free();
	assert(re.is_match("aa"));
	assert(re.is_match("bb"));
	assert(!re.is_match("ab"));
}

fn void test_branch_reset_following_group_numbering() @test
{
	Regex* re = regex::compile(mem, "(?|(a)|(b))(c)")!!;
	defer re.free();

	Match m1 = re.find(mem, "ac")!!;
	defer m1.free();
	assert(m1.group(1)!! == "a");
	assert(m1.group(2)!! == "c");

	Match m2 = re.find(mem, "bc")!!;
	defer m2.free();
	assert(m2.group(1)!! == "b");
	assert(m2.group(2)!! == "c");
}

fn void test_conditional_numeric() @test
{
	// (?(1)yes|no) — anchored so no sliding past the conditional
	Regex* re = regex::tcompile("^(\\d)?x(?(1)digit|other)$");
	test::@check(re.is_match("3xdigit"));
	test::@check(re.is_match("xother"));
	test::@check(!re.is_match("3xother"));
	test::@check(!re.is_match("xdigit"));
}

fn void test_conditional_named() @test
{
	// (?(name)yes|no) — named group condition
	Regex* re = regex::tcompile("(?<n>\\d)?x(?(n)digit|other)");
	test::@check(re.is_match("3xdigit"));
	test::@check(re.is_match("xother"));
}

fn void test_conditional_no_else() @test
{
	// (?(1)yes) — no else branch: condition failure → match empty
	// Anchored to prevent finding match at a later position
	Regex* re = regex::tcompile("^(a)?(?(1)b)c$");
	test::@check(re.is_match("abc"));
	test::@check(re.is_match("c"));   // no 'a', no 'b', condition fails → empty → matches 'c'
	test::@check(!re.is_match("ac")); // 'a' captured but no 'b'
}

fn void test_conditional_preserves_following_captures() @test
{
	Regex* re = regex::compile(mem, "^(a)?(?(1)b|c)(d)$")!!;
	defer re.free();

	Match m1 = re.find(mem, "abd")!!;
	defer m1.free();
	assert(m1.group(1)!! == "a");
	assert(m1.group(2)!! == "d");

	Match m2 = re.find(mem, "cd")!!;
	defer m2.free();
	assert(@catch(m2.group(1)) == regex::GROUP_NOT_FOUND);
	assert(m2.group(2)!! == "d");

	assert(!re.is_match("acd"));
	assert(!re.is_match("bd"));
}

fn void test_gpos_anchor() @test
{
	// \G anchors to the position where find_at starts
	@pool()
	{
		Regex* re = regex::tcompile("\\G\\d+");
		// find_at(0): \G matches at pos 0, digits start there
		Match? m1 = re.find_at(tmem, "123abc", 0);
		test::@check(@ok(m1));
		if (try m1) { test::@check(m1.full() == "123"); m1.free(); }
		// find_at(3): pos 3 is 'a', \G matches but \d+ fails
		Match? m2 = re.find_at(tmem, "123abc", 3);
		test::@check(!@ok(m2));
	};
}

fn void test_gpos_find_all() @test
{
	// \G\w+ tokenizes input by always matching at the previous end
	@pool()
	{
		Regex* re = regex::tcompile("\\G\\w+");
		MatchIter it = re.find_all(tmem, "hello world");
		Match? m1 = it.next();
		test::@check(@ok(m1));
		if (try m1) { test::@check(m1.full() == "hello"); m1.free(); }
		// After "hello", pos=5; input[5]=' '; \G at pos=5, \w+ fails → no more matches
		Match? m2 = it.next();
		test::@check(!@ok(m2));
	};
}

fn void test_gpos_find_all_progresses_from_previous_end() @test
{
	Regex* re = regex::compile(mem, "\\G\\d{2}")!!;
	defer re.free();
	MatchIter it = re.find_all(mem, "123456x");

	Match m1 = it.next()!!;
	defer m1.free();
	assert(m1.full() == "12");

	Match m2 = it.next()!!;
	defer m2.free();
	assert(m2.full() == "34");

	Match m3 = it.next()!!;
	defer m3.free();
	assert(m3.full() == "56");

	assert(@catch(it.next()) == regex::NO_MATCH);
}
