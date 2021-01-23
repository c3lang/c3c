module levenshtein;

func int levenshtein(string s, string t)
{
    // if either string is empty, difference is inserting all chars
    // from the other
    if (!s.size) return t.size;
    if (!t.size) return s.size;

    // if last letters are the same, the difference is whatever is
    // required to edit the rest of the strings
     if (s[s.size - 1] == t[t.size - 1]) return levenshtein(s.slice(0, s.size - 1), t.slice(0, t.size - 1));

     // else try:
     // changing last letter of s to that of t; or
     // remove last letter of s; or
     // remove last letter of t,
     // any of which is 1 edit plus editing the rest of the strings
     int a = levenshtein(s.slice(0, s.size - 1), t.slice(0, t.size - 1));
     int b = levenshtein(s, t.slice(0, t.size - 1);
     int c = levenshtein(s.slice(0, s.size - 1), t);

     return @max(@max(a, b), c) + 1;
}