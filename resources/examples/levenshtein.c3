module levenshtein;
import std::math;

// This levenshtein exercises C3 slices.
fn int levenshtein(String s, String t)
{
	// if either string is empty, difference is inserting all chars
	// from the other
	if (!s.len) return t.len;
	if (!t.len) return s.len;

	// if last letters are the same, the difference is whatever is
	// required to edit the rest of the strings
	if (s[^1] == t[^1]) return levenshtein(s[0..^2], t[0..^2]);

	// else try:
	// changing last letter of s to that of t; or
	// remove last letter of s; or
	// remove last letter of t,
	// any of which is 1 edit plus editing the rest of the strings
	int a = levenshtein(s[0..^2], t[0..^2]);
	int b = levenshtein(s, t[0..^2]);
	int c = levenshtein(s[0..^2], t);

	return min(min(a, b), c) + 1;
}