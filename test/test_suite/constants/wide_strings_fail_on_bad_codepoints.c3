ushort[] b = u"😀😀😀"; // #error: Invalid codepoint in string: a 16-bit wide-string cannot accommodate codepoints over U+FFFF.
ushort[] b = u"ascii 𝄞 around"; // #error: Invalid codepoint in string: a 16-bit wide-string cannot accommodate codepoints over U+FFFF.
