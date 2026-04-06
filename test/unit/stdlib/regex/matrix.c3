module regex_matrix_test;
import std::regex;

fn void systematic_api_matrix() @test
{
	ApiCase[] cases = {
		{ .name = "literal-two-hits", .pattern = "world", .flags = regex::FLAG_NONE, .input = "hello world world", .matched = true, .start = 6, .full = "world", .find_all_count = 2 },
		{ .name = "literal-miss", .pattern = "world", .flags = regex::FLAG_NONE, .input = "hello there", .matched = false, .start = 0, .full = "", .find_all_count = 0 },
		{ .name = "nullable-prefix", .pattern = "a*bc", .flags = regex::FLAG_NONE, .input = "xxbcabc", .matched = true, .start = 2, .full = "bc", .find_all_count = 2 },
		{ .name = "word-boundary-zero-length", .pattern = "\\b", .flags = regex::FLAG_NONE, .input = "ab cd", .matched = true, .start = 0, .full = "", .find_all_count = 4 },
		{ .name = "multiline-anchor", .pattern = "^foo", .flags = regex::FLAG_MULTILINE, .input = "bar\nfoo\nfoo", .matched = true, .start = 4, .full = "foo", .find_all_count = 2 },
		{ .name = "unicode-word-boundary", .pattern = "\\bcaf\u00E9\\b", .flags = regex::FLAG_NONE, .input = "le caf\u00E9 noir caf\u00E9!", .matched = true, .start = 3, .full = "caf\u00E9", .find_all_count = 2 },
		{ .name = "lookahead-dollars", .pattern = "\\d+(?= dollars)", .flags = regex::FLAG_NONE, .input = "100 dollars and 250 dollars", .matched = true, .start = 0, .full = "100", .find_all_count = 2 },
		{ .name = "lookbehind-currency", .pattern = "(?<=\\$)\\d+", .flags = regex::FLAG_NONE, .input = "price $42 and $7", .matched = true, .start = 7, .full = "42", .find_all_count = 2 },
		{ .name = "backref-repeated-word", .pattern = "(\\w+) \\1", .flags = regex::FLAG_NONE, .input = "hello hello test test", .matched = true, .start = 0, .full = "hello hello", .find_all_count = 2 },
		{ .name = "atomic-unanchored", .pattern = "(?>a|ab)c", .flags = regex::FLAG_NONE, .input = "abc ac", .matched = true, .start = 4, .full = "ac", .find_all_count = 1 },
		{ .name = "conditional-no-prefix", .pattern = "^(\\d)?x(?(1)digit|other)$", .flags = regex::FLAG_NONE, .input = "xother", .matched = true, .start = 0, .full = "xother", .find_all_count = 1 },
		{ .name = "unicode-whitespace-property", .pattern = "\\p{White_Space}+x", .flags = regex::FLAG_NONE, .input = "a\u00A0x \u2028x", .matched = true, .start = 1, .full = "\u00A0x", .find_all_count = 2 },
		{ .name = "inline-compat-parse-only", .pattern = "ab(?i)cd", .flags = regex::FLAG_NONE, .input = "abCD abcd", .matched = true, .start = 5, .full = "abcd", .find_all_count = 1 },
		{ .name = "dotall-runtime-flag", .pattern = "a.b", .flags = regex::FLAG_DOTALL, .input = "a\nb aXb", .matched = true, .start = 0, .full = "a\nb", .find_all_count = 2 },
	};

	foreach (c : cases) assert_api_case(c);
}

fn void systematic_find_at_matrix() @test
{
	FindAtCase[] cases = {
		{ .name = "literal-forward-scan", .pattern = "world", .flags = regex::FLAG_NONE, .input = "hello world", .start_pos = 2, .matched = true, .match_start = 6, .full = "world" },
		{ .name = "nullable-prefix-skips-earlier-hit", .pattern = "a*bc", .flags = regex::FLAG_NONE, .input = "xxbcabc", .start_pos = 3, .matched = true, .match_start = 4, .full = "abc" },
		{ .name = "multiline-anchor-find-at", .pattern = "^foo", .flags = regex::FLAG_MULTILINE, .input = "bar\nfoo", .start_pos = 1, .matched = true, .match_start = 4, .full = "foo" },
		{ .name = "gpos-start-zero", .pattern = "\\G\\d{2}", .flags = regex::FLAG_NONE, .input = "123456x", .start_pos = 0, .matched = true, .match_start = 0, .full = "12" },
		{ .name = "gpos-midstream", .pattern = "\\G\\d{2}", .flags = regex::FLAG_NONE, .input = "123456x", .start_pos = 2, .matched = true, .match_start = 2, .full = "34" },
		{ .name = "gpos-no-match", .pattern = "\\G\\d{2}", .flags = regex::FLAG_NONE, .input = "123456x", .start_pos = 5, .matched = false, .match_start = 0, .full = "" },
		{ .name = "boundary-search-from-middle", .pattern = "\\b", .flags = regex::FLAG_NONE, .input = "ab cd", .start_pos = 1, .matched = true, .match_start = 2, .full = "" },
		{ .name = "inline-compat-forward-scan", .pattern = "ab(?i)cd", .flags = regex::FLAG_NONE, .input = "abCD abcd", .start_pos = 1, .matched = true, .match_start = 5, .full = "abcd" },
	};

	foreach (c : cases) assert_find_at_case(c);
}

fn void systematic_capture_matrix() @test
{
	CaptureCase[] cases = {
		{
			.name = "email-numbered",
			.pattern = "(\\w+)@(\\w+)",
			.input = "user@example",
			.full = "user@example",
			.g1_present = true, .g1 = "user",
			.g2_present = true, .g2 = "example",
			.g3_present = false, .g3 = "",
		},
		{
			.name = "date-named",
			.pattern = "(?<year>\\d{4})-(?<month>\\d{2})-(?<day>\\d{2})",
			.input = "date: 2024-03-15",
			.full = "2024-03-15",
			.g1_present = true, .g1 = "2024",
			.g2_present = true, .g2 = "03",
			.g3_present = true, .g3 = "15",
			.named1_name = "year", .named1_present = true, .named1_value = "2024",
			.named2_name = "month", .named2_present = true, .named2_value = "03",
			.named3_name = "day", .named3_present = true, .named3_value = "15",
		},
		{
			.name = "branch-reset-ac",
			.pattern = "(?|(a)|(b))(c)",
			.input = "ac",
			.full = "ac",
			.g1_present = true, .g1 = "a",
			.g2_present = true, .g2 = "c",
			.g3_present = false, .g3 = "",
		},
		{
			.name = "branch-reset-bc",
			.pattern = "(?|(a)|(b))(c)",
			.input = "bc",
			.full = "bc",
			.g1_present = true, .g1 = "b",
			.g2_present = true, .g2 = "c",
			.g3_present = false, .g3 = "",
		},
		{
			.name = "conditional-else-branch",
			.pattern = "^(a)?(?(1)b|c)(d)$",
			.input = "cd",
			.full = "cd",
			.g1_present = false, .g1 = "",
			.g2_present = true, .g2 = "d",
			.g3_present = false, .g3 = "",
		},
		{
			.name = "conditional-then-branch",
			.pattern = "^(a)?(?(1)b|c)(d)$",
			.input = "abd",
			.full = "abd",
			.g1_present = true, .g1 = "a",
			.g2_present = true, .g2 = "d",
			.g3_present = false, .g3 = "",
		},
		{
			.name = "keep-preserves-capture",
			.pattern = "(foo)\\K(bar)",
			.input = "foobar",
			.full = "bar",
			.g1_present = true, .g1 = "foo",
			.g2_present = true, .g2 = "bar",
			.g3_present = false, .g3 = "",
		},
	};

	foreach (c : cases) assert_capture_case(c);
}

fn void systematic_find_all_sequence_matrix() @test
{
	FindAllSequenceCase[] cases = {
		{
			.name = "literal-sequence",
			.pattern = "world",
			.flags = regex::FLAG_NONE,
			.input = "hello world world",
			.starts = { 6, 12 },
			.fulls = { "world", "world" },
		},
		{
			.name = "word-boundary-zero-length-sequence",
			.pattern = "\\b",
			.flags = regex::FLAG_NONE,
			.input = "ab cd",
			.starts = { 0, 2, 3, 5 },
			.fulls = { "", "", "", "" },
		},
		{
			.name = "gpos-sequence",
			.pattern = "\\G\\d{2}",
			.flags = regex::FLAG_NONE,
			.input = "123456x",
			.starts = { 0, 2, 4 },
			.fulls = { "12", "34", "56" },
		},
		{
			.name = "keep-sequence",
			.pattern = "foo\\K(bar|qux)",
			.flags = regex::FLAG_NONE,
			.input = "foobar fooqux",
			.starts = { 3, 10 },
			.fulls = { "bar", "qux" },
		},
		{
			.name = "unicode-boundary-sequence",
			.pattern = "\\bcaf\u00E9\\b",
			.flags = regex::FLAG_NONE,
			.input = "caf\u00E9 noir caf\u00E9!",
			.starts = { 0, 11 },
			.fulls = { "caf\u00E9", "caf\u00E9" },
		},
	};

	foreach (c : cases) assert_find_all_sequence_case(c);
}

fn void systematic_replace_matrix() @test
{
	ReplaceCase[] cases = {
		{
			.name = "literal-single-vs-all",
			.pattern = "world",
			.flags = regex::FLAG_NONE,
			.input = "hello world world",
			.replacement = "C3",
			.replace_matches = true,
			.replace_expected = "hello C3 world",
			.replace_all_expected = "hello C3 C3",
		},
		{
			.name = "digits-single-vs-all",
			.pattern = "\\d+",
			.flags = regex::FLAG_NONE,
			.input = "a1b22c333",
			.replacement = "X",
			.replace_matches = true,
			.replace_expected = "aXb22c333",
			.replace_all_expected = "aXbXcX",
		},
		{
			.name = "numbered-captures",
			.pattern = "(\\w+)@(\\w+)",
			.flags = regex::FLAG_NONE,
			.input = "user@example admin@test",
			.replacement = "$2/$1",
			.replace_matches = true,
			.replace_expected = "example/user admin@test",
			.replace_all_expected = "example/user test/admin",
		},
		{
			.name = "named-captures",
			.pattern = "(?<first>\\w+) (?<last>\\w+)",
			.flags = regex::FLAG_NONE,
			.input = "John Doe Jane Roe",
			.replacement = "${last}, ${first}",
			.replace_matches = true,
			.replace_expected = "Doe, John Jane Roe",
			.replace_all_expected = "Doe, John Roe, Jane",
		},
		{
			.name = "literal-dollar",
			.pattern = "\\d+",
			.flags = regex::FLAG_NONE,
			.input = "a1b2",
			.replacement = "$$$0",
			.replace_matches = true,
			.replace_expected = "a$1b2",
			.replace_all_expected = "a$1b$2",
		},
		{
			.name = "missing-groups-expand-empty",
			.pattern = "(a)?b",
			.flags = regex::FLAG_NONE,
			.input = "b b",
			.replacement = "<$1><${9}><${missing}>",
			.replace_matches = true,
			.replace_expected = "<><><> b",
			.replace_all_expected = "<><><> <><><>",
		},
		{
			.name = "replace-no-match-fault",
			.pattern = "world",
			.flags = regex::FLAG_NONE,
			.input = "hello there",
			.replacement = "C3",
			.replace_matches = false,
			.replace_expected = "",
			.replace_all_expected = "hello there",
		},
	};

	foreach (c : cases) assert_replace_case(c);
}

fn void systematic_compile_contract_matrix() @test
{
	String[] valid_patterns = {
		"abc",
		"(?x) a b c",
		"(?U)<.+>",
		"(?i)abc",
		"(?-i)abc",
		"(?m)^foo",
		"(?s)a.b",
		"(?|(a)|(b))(c)",
		"(\\w+) \\1",
		"(?>a|ab)c",
		"^(\\d)?x(?(1)digit|other)$",
		"\\G\\d{2}",
		"foo\\Kbar",
		"(?<=\\w+)\\d+",
		"\\p{Word}+",
	};
	foreach (pattern : valid_patterns)
	{
		Regex*? re = regex::compile(mem, pattern);
		test::@check(@ok(re), "compile matrix: expected valid pattern %s to compile", pattern);
		if (try re) re.free();
	}

	String[] invalid_patterns = {
		"(?i:foo)",
		"(?x: a b c )",
		"^*",
		"(?=a)+",
		"(?<!a){2}",
		"\\p{Letter}",
		"[\\P{Letter}]",
		"(abc",
	};
	foreach (pattern : invalid_patterns)
	{
		Regex*? re = regex::compile(mem, pattern);
		test::@check(!@ok(re), "compile matrix: expected invalid pattern %s to fail", pattern);
		if (try re) re.free();
	}

	test::@check(@catch(regex::compile(mem, "a{5000}")) == regex::COMPILE_ERROR, "compile matrix: oversized repeat should be COMPILE_ERROR");
}

struct ApiCase
{
	String pattern;
	RegexFlags flags;
	String input;
	bool matched;
	usz start;
	String full;
	int find_all_count;
	String name;
}

struct FindAtCase
{
	String pattern;
	RegexFlags flags;
	String input;
	usz start_pos;
	bool matched;
	usz match_start;
	String full;
	String name;
}

struct FindAllSequenceCase
{
	String pattern;
	RegexFlags flags;
	String input;
	usz[] starts;
	String[] fulls;
	String name;
}

struct CaptureCase
{
	String pattern;
	String input;
	String full;
	String name;
	bool g1_present;
	String g1;
	bool g2_present;
	String g2;
	bool g3_present;
	String g3;
	String named1_name;
	bool named1_present;
	String named1_value;
	String named2_name;
	bool named2_present;
	String named2_value;
	String named3_name;
	bool named3_present;
	String named3_value;
}

struct ReplaceCase
{
	String pattern;
	RegexFlags flags;
	String input;
	String replacement;
	bool replace_matches;
	String replace_expected;
	String replace_all_expected;
	String name;
}

fn void assert_api_case(ApiCase c) @local
{
	Regex* re = regex::compile(mem, c.pattern, c.flags)!!;
	defer re.free();

	test::@check(re.is_match(c.input) == c.matched, "api case %s: is_match mismatch", c.name);

	Match? first_find = re.find(mem, c.input);
	if (!c.matched)
	{
		test::@check(@catch(first_find) == regex::NO_MATCH, "api case %s: expected find() to fail", c.name);
	}
	else
	{
		test::@check(@ok(first_find), "api case %s: expected find() to succeed", c.name);
		if (try first_find)
		{
			defer first_find.free();
			test::@check(first_find.start() == c.start, "api case %s: find() start mismatch", c.name);
			test::@check(first_find.full() == c.full, "api case %s: find() full mismatch", c.name);
			test::@check(first_find.end() == c.start + c.full.len, "api case %s: find() end mismatch", c.name);
		}
	}

	Match? first_find_at = re.find_at(mem, c.input, 0);
	if (!c.matched)
	{
		test::@check(@catch(first_find_at) == regex::NO_MATCH, "api case %s: expected find_at(0) to fail", c.name);
	}
	else
	{
		test::@check(@ok(first_find_at), "api case %s: expected find_at(0) to succeed", c.name);
		if (try first_find_at)
		{
			defer first_find_at.free();
			test::@check(first_find_at.start() == c.start, "api case %s: find_at(0) start mismatch", c.name);
			test::@check(first_find_at.full() == c.full, "api case %s: find_at(0) full mismatch", c.name);
		}

		Match from_start = re.find_at(mem, c.input, c.start)!!;
		defer from_start.free();
		test::@check(from_start.start() == c.start, "api case %s: find_at(start) mismatch", c.name);
		test::@check(from_start.full() == c.full, "api case %s: find_at(start) full mismatch", c.name);
	}

	String? replaced_once = re.replace(mem, c.input, "$&");
	if (!c.matched)
	{
		test::@check(@catch(replaced_once) == regex::NO_MATCH, "api case %s: replace($&) should fail with NO_MATCH", c.name);
	}
	else
	{
		String replace_identity = replaced_once!!;
		defer alloc::free(mem, replace_identity.ptr);
		test::@check(replace_identity == c.input, "api case %s: replace($&) should preserve input", c.name);
	}

	String replaced_all = re.replace_all(mem, c.input, "$&")!!;
	defer alloc::free(mem, replaced_all.ptr);
	test::@check(replaced_all == c.input, "api case %s: replace_all($&) should preserve input", c.name);

	MatchIter it = re.find_all(mem, c.input);
	int count = 0;
	bool checked_first = false;
	usz last_end = 0;
	while (try m = it.next())
	{
		defer m.free();
		test::@check(m.start() >= last_end, "api case %s: find_all order regression", c.name);
		test::@check(m.end() >= m.start(), "api case %s: find_all invalid range", c.name);
		if (!checked_first)
		{
			test::@check(m.start() == c.start, "api case %s: find_all first start mismatch", c.name);
			test::@check(m.full() == c.full, "api case %s: find_all first full mismatch", c.name);
			checked_first = true;
		}
		last_end = m.end();
		count++;
		if (count > 64)
		{
			test::@check(false, "api case %s: find_all exceeded sanity limit", c.name);
			break;
		}
	}
	test::@check(count == c.find_all_count, "api case %s: find_all count mismatch", c.name);
}

fn void assert_find_at_case(FindAtCase c) @local
{
	Regex* re = regex::compile(mem, c.pattern, c.flags)!!;
	defer re.free();

	Match? m = re.find_at(mem, c.input, c.start_pos);
	if (!c.matched)
	{
		test::@check(@catch(m) == regex::NO_MATCH, "find_at case %s: expected NO_MATCH", c.name);
		return;
	}

	test::@check(@ok(m), "find_at case %s: expected match", c.name);
	if (try m)
	{
		defer m.free();
		test::@check(m.start() == c.match_start, "find_at case %s: start mismatch", c.name);
		test::@check(m.full() == c.full, "find_at case %s: full mismatch", c.name);
		test::@check(m.end() == c.match_start + c.full.len, "find_at case %s: end mismatch", c.name);
	}
}

fn void assert_find_all_sequence_case(FindAllSequenceCase c) @local
{
	test::@check(c.starts.len == c.fulls.len, "find_all sequence %s: invalid fixture", c.name);

	Regex* re = regex::compile(mem, c.pattern, c.flags)!!;
	defer re.free();

	MatchIter it = re.find_all(mem, c.input);
	usz index = 0;
	while (try m = it.next())
	{
		defer m.free();
		test::@check(index < c.starts.len, "find_all sequence %s: unexpected extra match", c.name);
		if (index >= c.starts.len) break;
		test::@check(m.start() == c.starts[index], "find_all sequence %s: start mismatch at %d", c.name, index);
		test::@check(m.full() == c.fulls[index], "find_all sequence %s: full mismatch at %d", c.name, index);
		index++;
	}
	test::@check(index == c.starts.len, "find_all sequence %s: missing matches", c.name);
}

fn void assert_capture_case(CaptureCase c) @local
{
	Regex* re = regex::compile(mem, c.pattern)!!;
	defer re.free();
	Match m = re.find(mem, c.input)!!;
	defer m.free();

	test::@check(m.full() == c.full, "capture case %s: full mismatch", c.name);

	if (c.g1_present)
	{
		test::@check(m.group(1)!! == c.g1, "capture case %s: group 1 mismatch", c.name);
	}
	else
	{
		test::@check(@catch(m.group(1)) == regex::GROUP_NOT_FOUND, "capture case %s: group 1 should be absent", c.name);
	}

	if (c.g2_present)
	{
		test::@check(m.group(2)!! == c.g2, "capture case %s: group 2 mismatch", c.name);
	}
	else
	{
		test::@check(@catch(m.group(2)) == regex::GROUP_NOT_FOUND, "capture case %s: group 2 should be absent", c.name);
	}

	if (c.g3_present)
	{
		test::@check(m.group(3)!! == c.g3, "capture case %s: group 3 mismatch", c.name);
	}
	else
	{
		test::@check(@catch(m.group(3)) == regex::GROUP_NOT_FOUND, "capture case %s: group 3 should be absent", c.name);
	}

	if (c.named1_name.len > 0)
	{
		if (c.named1_present)
		{
			test::@check(m.named_group(c.named1_name)!! == c.named1_value, "capture case %s: named group 1 mismatch", c.name);
		}
		else
		{
			test::@check(@catch(m.named_group(c.named1_name)) == regex::GROUP_NOT_FOUND, "capture case %s: named group 1 should be absent", c.name);
		}
	}
	if (c.named2_name.len > 0)
	{
		if (c.named2_present)
		{
			test::@check(m.named_group(c.named2_name)!! == c.named2_value, "capture case %s: named group 2 mismatch", c.name);
		}
		else
		{
			test::@check(@catch(m.named_group(c.named2_name)) == regex::GROUP_NOT_FOUND, "capture case %s: named group 2 should be absent", c.name);
		}
	}
	if (c.named3_name.len > 0)
	{
		if (c.named3_present)
		{
			test::@check(m.named_group(c.named3_name)!! == c.named3_value, "capture case %s: named group 3 mismatch", c.name);
		}
		else
		{
			test::@check(@catch(m.named_group(c.named3_name)) == regex::GROUP_NOT_FOUND, "capture case %s: named group 3 should be absent", c.name);
		}
	}
}

fn void assert_replace_case(ReplaceCase c) @local
{
	Regex* re = regex::compile(mem, c.pattern, c.flags)!!;
	defer re.free();

	String? replaced = re.replace(mem, c.input, c.replacement);
	if (!c.replace_matches)
	{
		test::@check(@catch(replaced) == regex::NO_MATCH, "replace case %s: replace should return NO_MATCH", c.name);
	}
	else
	{
		String replaced_once = replaced!!;
		defer alloc::free(mem, replaced_once.ptr);
		test::@check(replaced_once == c.replace_expected, "replace case %s: replace mismatch", c.name);
	}

	String replaced_all = re.replace_all(mem, c.input, c.replacement)!!;
	defer alloc::free(mem, replaced_all.ptr);
	test::@check(replaced_all == c.replace_all_expected, "replace case %s: replace_all mismatch", c.name);
}
