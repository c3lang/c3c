module regex_fuzz;
import std::regex;
import std::math::random;

fn void regex_corpus_fuzz() @test
{
	DefaultRandom rng;
	random::seed(&rng, 0xC3C0_0001uL);

	for (int i = 0; i < 250; i++)
	{
		@pool()
		{
			String pattern = build_corpus_pattern(&rng);
			String input = build_random_input(&rng);
			Regex*? re = regex::compile(tmem, pattern);
			if (catch err = re)
			{
				assert(err == regex::INVALID_PATTERN || err == regex::COMPILE_ERROR);
				continue;
			}
			assert_match_invariants(re, input, &rng);
		};
	}
}

fn void regex_parser_noise_fuzz() @test
{
	DefaultRandom rng;
	random::seed(&rng, 0xC3C0_0002uL);

	for (int i = 0; i < 400; i++)
	{
		@pool()
		{
			String pattern = build_parser_noise(&rng);
			String input = build_random_input(&rng);
			Regex*? re = regex::compile(tmem, pattern);
			if (catch err = re)
			{
				assert(err == regex::INVALID_PATTERN || err == regex::COMPILE_ERROR);
				continue;
			}
			assert_match_invariants(re, input, &rng);
		};
	}
}

const String[] VALID_PATTERN_CORPUS = {
	"a",
	"ab",
	"abc",
	"\u00E9",
	"\u03A9",
	"\u0416",
	"\\d+",
	"\\w+",
	"\\s+",
	"\\h+",
	"\\v+",
	"[ab]+",
	"[^c]*",
	"(?:ab|ba)",
	"(a|ab)c",
	"(?=a)a",
	"(?!b).",
	"(?<=a)b",
	"(?<!b)a",
	"(?>a|ab)c",
	"(a)\\1",
	"(?|(a)|(b))\\1",
	"foo\\Kbar",
	"\\bcat\\b",
	"\\G\\d{2}",
	"(?<word>\\w+)=(?<value>\\d+)",
	"(?<=\\$)\\d+",
	"\\p{Word}+",
	"\\p{White_Space}+",
	"\\R",
	"^foo$",
	"(?U)<.+>",
};

const String[] INPUT_CORPUS = {
	"",
	"a",
	"ab",
	"abc",
	"abba",
	"foo",
	"foobar",
	"bar",
	"cat",
	"dog",
	"12",
	"1234",
	"hello",
	"world",
	" ",
	"-",
	"_",
	"\u00E9",
	"\u03C9",
	"\u0436",
	"\u00A0",
	"\u2028",
	"foo\nbar",
	"hello world",
	"$42",
	"foo=123",
};

const String[] REPLACEMENT_CORPUS = {
	"$&",
	"$0",
	"${0}",
	"${1}",
	"${9}",
	"<$0>",
	"<$1>",
	"<${missing}>",
	"$$",
	"$$$0",
	"${value}:${word}",
	"X",
	"_",
	"",
};

const String PATTERN_NOISE_ALPHABET = "abc01[](){}?+*|\\<>:=!^$.-";

fn String pick_string(String[] options, DefaultRandom* rng) @local
{
	usz index = (usz)random::next(rng, (int)options.len);
	return options[index];
}

fn String build_corpus_pattern(DefaultRandom* rng) @local
{
	DString pattern = dstring::temp_with_capacity(96);
	String base = pick_string(VALID_PATTERN_CORPUS, rng);
	switch (random::next(rng, 6))
	{
		case 0:
			pattern.append(base);
		case 1:
			pattern.append("^");
			pattern.append(base);
			pattern.append("$");
		case 2:
			pattern.append("(?:");
			pattern.append(base);
			pattern.append(")");
		case 3:
			pattern.append(base);
			pattern.append("|");
			pattern.append(pick_string(VALID_PATTERN_CORPUS, rng));
		case 4:
			pattern.append("(");
			pattern.append(base);
			pattern.append(")");
			pattern.append(random::next_bool(rng) ? "+" : "?");
		default:
			pattern.append(random::next_bool(rng) ? "(?x)" : "(?U)");
			pattern.append(base);
	}
	return pattern.str_view();
}

fn String build_random_input(DefaultRandom* rng) @local
{
	DString input = dstring::temp_with_capacity(64);
	int parts = random::next_in_range(rng, 0, 6);
	for (int i = 0; i < parts; i++) input.append(pick_string(INPUT_CORPUS, rng));
	return input.str_view();
}

fn String build_parser_noise(DefaultRandom* rng) @local
{
	DString pattern = dstring::temp_with_capacity(48);
	int len = random::next_in_range(rng, 0, 18);
	for (int i = 0; i < len; i++)
	{
		usz index = (usz)random::next(rng, PATTERN_NOISE_ALPHABET.len);
		pattern.append_char(PATTERN_NOISE_ALPHABET[index]);
	}
	return pattern.str_view();
}

fn void assert_match_invariants(Regex* re, String input, DefaultRandom* rng) @local
{
	bool matched = re.is_match(input);
	Match? first = re.find(tmem, input);
	assert(matched == @ok(first));
	if (try first)
	{
		assert(first.full() == input[first.start() : first.end() - first.start()]);
		first.free();
	}

	String? identity_once = re.replace(tmem, input, "$&");
	if (matched)
	{
		assert(identity_once!! == input);
	}
	else
	{
		assert(@catch(identity_once) == regex::NO_MATCH);
	}

	String identity_all = re.replace_all(tmem, input, "$&")!!;
	assert(identity_all == input);

	String replacement = pick_string(REPLACEMENT_CORPUS, rng);
	String replaced_all = re.replace_all(tmem, input, replacement)!!;
	assert(replaced_all.len >= 0);

	MatchIter it = re.find_all(tmem, input);
	usz last_end = 0;
	int seen = 0;
	while (seen < 16)
	{
		Match? next = it.next();
		if (catch next) break;
		assert(next.start() >= last_end);
		assert(next.end() >= next.start());
		last_end = next.end();
		next.free();
		seen++;
	}
}
